{-# LANGUAGE GADTs #-}
-- | Check that all type constructor are applied to the right number of
-- variables and all variables are bound.
module Pukeko.Language.KindChecker
  ( checkModule
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.ST
import           Data.Foldable
import           Data.Forget      (Forget (..))
import           Data.Maybe       (catMaybes)
import qualified Data.Map         as Map
import           Data.STRef
import           Data.Traversable
import qualified Data.Vector.Sized as Vec

import           Pukeko.Error
import           Pukeko.Pretty
import           Pukeko.Language.AST.Std          hiding (Free)
import qualified Pukeko.Language.AST.Stage        as St
import qualified Pukeko.Language.AST.ConDecl      as Con
import qualified Pukeko.Language.Ident            as Id
import           Pukeko.Language.Type

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

type KCEnv n s = Vec.Vector n (Kind (Open s))

data KCState s = MkKCState
  { _typeCons :: Map.Map Id.TCon (Kind (Open s))
  , _fresh    :: [Id.TVar]
  }
makeLenses ''KCState

newtype KC n s a =
  KC{unKC :: ReaderT (KCEnv n s) (StateT (KCState s) (ExceptT String (ST s))) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (KCEnv n s)
           , MonadState (KCState s)
           )

runKC :: MonadError String m => (forall n s. KC n s a) -> m a
runKC kc = runST (runExceptT (evalStateT (runReaderT (unKC kc) env0) st0))
  where
    st0 = MkKCState
      { _typeCons = Map.empty
      , _fresh    = Id.freshTVars
      }
    env0 = Vec.empty

liftST :: ST s a -> KC n s a
liftST = KC . lift . lift . lift

freshUVar :: KC n s (Kind (Open s))
freshUVar = do
  v <- fresh %%= (\(v:vs) -> (v, vs))
  UVar <$> liftST (newSTRef (Free (Forget v)))

localize :: Vec.Vector n (Kind (Open s)) -> KC n s a -> KC m s a
localize env = KC . withReaderT (const env) . unKC

kcType :: Kind (Open s) -> Type (TFinScope n Void) -> KC n s ()
kcType k = \case
  TVar v -> do
    kv <- asks (Vec.! scope id absurd v)
    unify kv k
  TArr -> unify (Arrow Star (Arrow Star Star)) k
  TCon tcon -> do
    kcon_opt <- use (typeCons . at tcon)
    case kcon_opt of
      Nothing -> bugWith "unknown type constructor" tcon
      Just kcon -> unify kcon k
  TApp tf tp -> do
    ktp <- freshUVar
    kcType ktp tp
    kcType (Arrow ktp k) tf
  TUni _ _ -> bug "universal quantificatio"

kcTypDef :: [Some1 Con.TConDecl] -> KC n s ()
kcTypDef tcons = do
  kinds <- for tcons $ \(Some1 Con.MkTConDecl{_tname}) -> do
    kind <- freshUVar
    typeCons . at _tname ?= kind
    pure kind
  for_ (zip tcons kinds) $ \(Some1 Con.MkTConDecl{_params, _dcons}, tconKind) -> do
    paramKinds <- traverse (const freshUVar) _params
    unify tconKind (foldr Arrow Star paramKinds)
    localize paramKinds $ do
      for_ _dcons $ \Con.MkDConDecl{_fields} -> do
        traverse_ (kcType Star) _fields
  traverse_ close kinds

kcVal :: Type Void -> KC n s ()
kcVal = \case
  TUni xs t -> k xs t
  t         -> k Vec.empty (fmap absurd t)
  where
    k xs t = do
      env <- traverse (const freshUVar) xs
      localize env (kcType Star t)


kcTopLevel :: TopLevel In -> KC n s (Maybe (TopLevel Out))
kcTopLevel = \case
  TLTyp w tcons -> do
    here w (kcTypDef tcons)
    pure Nothing
  TLVal w _ t -> do
    here w (kcVal t)
    pure Nothing
  TLDef d -> yield (TLDef (retagDefn d))
  TLAsm   b a -> yield (TLAsm (retagBind b) a)
  where
    yield = pure . Just

kcModule ::Module In -> KC n s (Module Out)
kcModule = module2tops (fmap catMaybes . traverse kcTopLevel)

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
