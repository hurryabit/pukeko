{-# LANGUAGE GADTs #-}
-- | Check that all type constructor are applied to the right number of
-- variables and all variables are bound.
module Pukeko.FrontEnd.KindChecker
  ( checkModule
  ) where

import Pukeko.Prelude

import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Data.Forget      (Forget (..))
import qualified Data.List.NonEmpty as NE
import           Data.STRef
import qualified Data.Vector.Sized as Vec

import           Pukeko.Pretty
import           Pukeko.AST.SystemF    hiding (Free)
import qualified Pukeko.AST.Stage      as St
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Type

type In  = St.FunResolver
type Out = St.KindChecker

data Open s

data UVar s
  = Free (Forget Id.TVar)
  | Link (Kind (Open s))

data Kind a where
  Star  ::                     Kind a
  Arrow :: Kind a -> Kind a -> Kind a
  UVar  :: STRef s (UVar s) -> Kind (Open s)

type KCEnv n s = Vector n (Kind (Open s))

type KCState s = Map Id.TCon (Kind (Open s))

type KC n s = (RWST (KCEnv n s) () (KCState s) (SupplyT Id.TVar (ExceptT String (ST s))))

runKC :: MonadError String m => (forall n s. KC n s a) -> m a
runKC kc = runST (runExceptT (evalSupplyT (fst <$> evalRWST kc env0 mempty) sup0))
  where
    sup0 = Id.freshTVars
    env0 = Vec.empty

freshUVar :: KC n s (Kind (Open s))
freshUVar = do
  v <- fresh
  UVar <$> liftST (newSTRef (Free (Forget v)))

localize :: Vector n (Kind (Open s)) -> KC n s a -> KC m s a
localize env = withRWST (\_ -> (,) env)

kcType :: Kind (Open s) -> Type (TFinScope n Void) -> KC n s ()
kcType k = \case
  TVar v -> do
    kv <- asks (Vec.! scope absurd id v)
    unify kv k
  TArr -> unify (Arrow Star (Arrow Star Star)) k
  TCon tcon -> do
    kcon_opt <- use (at tcon)
    case kcon_opt of
      Nothing -> bugWith "unknown type constructor" tcon
      Just kcon -> unify kcon k
  TApp tf tp -> do
    ktp <- freshUVar
    kcType ktp tp
    kcType (Arrow ktp k) tf
  TUni _ _ -> bug "universal quantificatio"

kcTypDef :: NonEmpty (Some1 TConDecl) -> KC n s ()
kcTypDef tcons = do
  kinds <- for tcons $ \(Some1 MkTConDecl{_tcon2name = tname}) -> do
    kind <- freshUVar
    at tname ?= kind
    pure kind
  for_ (NE.zip tcons kinds) $
    \(Some1 MkTConDecl{_tcon2prms = prms, _tcon2dcons = dcons}, tconKind) -> do
      paramKinds <- traverse (const freshUVar) prms
      unify tconKind (foldr Arrow Star paramKinds)
      localize paramKinds $ do
        -- TODO: Rewrite this in terms of @forOf_@.
        for_ dcons $ \MkDConDecl{_dcon2pos = w, _dcon2flds = flds} -> do
          here w (traverse_ (kcType Star) flds)
  traverse_ close kinds

kcVal :: Type Void -> KC n s ()
kcVal = \case
  TUni xs t -> k xs t
  t         -> k Vec.empty (fmap absurd t)
  where
    k xs t = do
      env <- traverse (const freshUVar) xs
      localize env (kcType Star t)


kcDecl :: Decl In -> KC n s (Decl Out)
kcDecl = \case
  DType tcons -> do
    (kcTypDef tcons)
    pure (DType tcons)
  DSign (MkSignDecl w z t) -> do
    here w (kcVal t)
    pure (DSign (MkSignDecl w z t))
  -- FIXME: Check kinds in type class declarations and instance definitions.
  DClss c -> pure (DClss c)
  DInst i -> DInst <$> inst2defn (pure . retagDefn) i
  DDefn d -> pure (DDefn (retagDefn d))
  DPrim p -> pure (DPrim p)

kcModule ::Module In -> KC n s (Module Out)
kcModule = module2decls (traverse (\top -> reset *> kcDecl top))

checkModule :: MonadError String m => Module In -> m (Module Out)
checkModule module_ = runKC (kcModule module_)


unwind :: Kind (Open s) -> KC n s (Kind (Open s))
unwind = \case
  k0@(UVar uref) -> do
    uvar <- liftST (readSTRef uref)
    case uvar of
      Free _  -> pure k0
      Link k1 -> do
        k2 <- unwind k1
        liftST (writeSTRef uref (Link k2))
        pure k2
  k -> pure k

assertFree :: STRef s (UVar s) -> KC a s ()
assertFree uref = do
  uvar <- liftST (readSTRef uref)
  case uvar of
    Free _ -> pure ()
    Link _ -> bug "unwinding produced link"

occursCheck :: STRef s (UVar s) -> Kind (Open s) -> KC n s ()
occursCheck uref1 = \case
  Star        -> pure ()
  Arrow kf kp -> occursCheck uref1 kf *> occursCheck uref1 kp
  UVar uref2
    | uref1 == uref2 -> throwError "occurs check"
    | otherwise -> do
        uvar2 <- liftST (readSTRef uref2)
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
      liftST (writeSTRef uref1 (Link k2))
    (_, UVar _) -> unify k2 k1
    -- TODO: Improve error message.
    (_, _) -> do
      d1 <- liftST (prettyKind False k1)
      d2 <- liftST (prettyKind False k2)
      throwDoc $ "cannot unify kinds" <+> d1 <+> "and" <+> d2

close :: Kind (Open s) -> KC n s ()
close k = do
  k <- unwind k
  case k of
    Star -> pure ()
    Arrow kf kp -> close kf *> close kp
    UVar uref -> do
      assertFree uref
      liftST (writeSTRef uref (Link Star))

prettyKind :: Bool -> Kind (Open s) -> ST s Doc
prettyKind prec = \case
  Star -> pure (text "*")
  Arrow kf kp -> do
    df <- prettyKind True  kf
    dp <- prettyKind False kp
    pure $ maybeParens prettyNormal prec (df <+> "->" <+> dp)
  UVar uref -> do
    uvar <- readSTRef uref
    case uvar of
      Free (Forget v) -> pure (pretty v)
      Link k          -> prettyKind prec k
