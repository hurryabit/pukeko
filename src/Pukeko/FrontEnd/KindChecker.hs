{-# LANGUAGE GADTs #-}
-- | Check that all type constructor are applied to the right number of
-- variables and all variables are bound.
module Pukeko.FrontEnd.KindChecker
  ( checkModule
  ) where

import Pukeko.Prelude
import Pukeko.Pretty

import           Control.Monad.Freer.Supply
import           Control.Monad.ST
import           Data.Forget      (Forget (..))
import qualified Data.Map.Extended as Map
import           Data.STRef

import           Pukeko.AST.SystemF
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Type

type In  = Surface

data Open s

data UVar s
  = Free (Forget Int)
  | Link (Kind (Open s))

data Kind a where
  Star  ::                     Kind a
  Arrow :: Kind a -> Kind a -> Kind a
  UVar  :: STRef s (UVar s) -> Kind (Open s)

type KCEnv n s = Map (Name TVar) (Kind (Open s))

type KCState s = Map (Name TCon) (Kind (Open s))

type KC n s =
  Eff
  [ Reader (KCEnv n s), Reader SourcePos, Supply Int
  , State (KCState s), Error Failure, ST s
  ]

freshUVar :: KC n s (Kind (Open s))
freshUVar = do
  v <- fresh
  UVar <$> sendM (newSTRef (Free (Forget v)))

kcType :: Kind (Open s) -> Type -> KC n s ()
kcType k = \case
  TVar v -> do
    kv <- asks (Map.! v)
    unify kv k
  TAtm TAArr -> unify (Arrow Star (Arrow Star Star)) k
  TAtm TAInt -> unify Star k
  TAtm (TACon tcon) -> do
    kcon <- gets (Map.! tcon)
    unify kcon k
  TApp tf tp -> do
    ktp <- freshUVar
    kcType ktp tp
    kcType (Arrow ktp k) tf
  TUni{} -> impossible  -- we have only rank-1 types and no type annotations
  -- FIXME: The class constraint needs to be checked as well.
  TCtx _ t1 -> do
    unify Star k
    kcType Star t1

kcTConDecl :: TConDecl -> KC n s ()
kcTConDecl (MkTConDecl tcon prms dcons0) = here tcon $ do
  kind <- freshUVar
  modify (Map.insert tcon kind)
  paramKinds <- traverse (const freshUVar) prms
  unify kind (foldr Arrow Star paramKinds)
  let env = Map.fromList (zip prms paramKinds)
  local (const env) $
    case dcons0 of
      Left typ -> kcType Star typ
      Right dcons ->
        for_ dcons $ here' $ \MkDConDecl{_dcon2fields = flds} ->
          traverse_ (kcType Star) flds
  close kind

kcVal :: Type -> KC n s ()
kcVal t = do
  let (vs, t0) = unwindr _TUni' t
  uvars <- traverse (const freshUVar) vs
  let env = Map.fromList (zip vs uvars)
  local (const env) (kcType Star t0)

kcDecl :: Decl In -> KC n s ()
kcDecl = \case
  DType tcon -> kcTConDecl tcon
  DSign (MkSignDecl _ t) -> kcVal t
  DFunc{} -> pure ()
  DExtn{} -> pure ()
  -- FIXME: Check kinds in type class declarations and instance definitions.
  DClss{} -> pure ()
  DInst{} -> pure ()

checkModule :: Member (Error Failure) effs => Module In -> Eff effs ()
checkModule (MkModule decls) = either throwError pure $ runST $
  for_ decls (\decl -> kcDecl decl
                       & runReader Map.empty
                       & runReader (getPos decl)
                       & evalSupply [1 ..])
  & evalState mempty
  & runError
  & runM


unwind :: Kind (Open s) -> KC n s (Kind (Open s))
unwind k0 = case k0 of
  UVar uref ->
    sendM (readSTRef uref) >>= \case
      Free _  -> pure k0
      Link k1 -> do
        k2 <- unwind k1
        sendM (writeSTRef uref (Link k2))
        pure k2
  _ -> pure k0

assertFree :: STRef s (UVar s) -> KC a s ()
assertFree uref =
  sendM (readSTRef uref) >>= \case
    Free _ -> pure ()
    Link _ -> impossible  -- this is only called after unwinding

occursCheck :: STRef s (UVar s) -> Kind (Open s) -> KC n s ()
occursCheck uref1 = \case
  Star        -> pure ()
  Arrow kf kp -> occursCheck uref1 kf *> occursCheck uref1 kp
  UVar uref2
    | uref1 == uref2 -> throwHere "occurs check"
    | otherwise -> do
        uvar2 <- sendM (readSTRef uref2)
        case uvar2 of
          Link k2 -> occursCheck uref1 k2
          Free _  -> pure ()

unify :: Kind (Open s) -> Kind (Open s) -> KC n s ()
unify k1 k2 = do
  k1 <- unwind k1
  k2 <- unwind k2
  case (k1, k2) of
    (Star, Star) -> pure ()
    (Arrow kf1 kp1, Arrow kf2 kp2) -> unify kf1 kf2 *> unify kp1 kp2
    (UVar uref1, UVar uref2)
      | uref1 == uref2 -> pure ()
    (UVar uref1, _) -> do
      assertFree uref1
      occursCheck uref1 k2
      sendM (writeSTRef uref1 (Link k2))
    (_, UVar _) -> unify k2 k1
    -- TODO: Improve error message.
    (_, _) -> do
      d1 <- sendM (prettyKind False k1)
      d2 <- sendM (prettyKind False k2)
      throwHere ("cannot unify kinds" <+> d1 <+> "and" <+> d2)

close :: Kind (Open s) -> KC n s ()
close k = do
  k <- unwind k
  case k of
    Star -> pure ()
    Arrow kf kp -> close kf *> close kp
    UVar uref -> do
      assertFree uref
      sendM (writeSTRef uref (Link Star))

prettyKind :: Bool -> Kind (Open s) -> ST s (Doc ann)
prettyKind prec = \case
  Star -> pure "*"
  Arrow kf kp -> do
    df <- prettyKind True  kf
    dp <- prettyKind False kp
    pure $ maybeParens prec (df <+> "->" <+> dp)
  UVar uref -> do
    uvar <- readSTRef uref
    case uvar of
      Free (Forget v) -> pure ("_" <> pretty v)
      Link k          -> prettyKind prec k
