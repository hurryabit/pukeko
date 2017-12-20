{-# LANGUAGE GADTs #-}
-- | Check that all type constructor are applied to the right number of
-- variables and all variables are bound.
module Pukeko.Language.KindChecker
  ( KC.Module
  , checkModule
  )
where

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

import           Pukeko.Error
import           Pukeko.Pretty
import           Pukeko.Language.AST.Std          hiding (Free)
import qualified Pukeko.Language.Ident            as Id
import qualified Pukeko.Language.TypeResolver.AST as TR
import qualified Pukeko.Language.KindChecker.AST  as KC
import qualified Pukeko.Language.Type             as Ty

type Type = Ty.Type KC.TypeCon Ty.Closed

data Open s

data UVar s
  = Free (Forget Id.TVar)
  | Link (Kind (Open s))

data Kind a where
  Star  ::                     Kind a
  Arrow :: Kind a -> Kind a -> Kind a
  UVar  :: STRef s (UVar s) -> Kind (Open s)

type KCEnv s = Map.Map Id.TVar (Kind (Open s))

data KCState s = MkKCState
  { _typeCons :: Map.Map Id.Con (Kind (Open s))
  , _fresh    :: [Id.TVar]
  }
makeLenses ''KCState

newtype KC s a =
  KC{unKC :: ReaderT (KCEnv s) (StateT (KCState s) (ExceptT String (ST s))) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader (KCEnv s)
           , MonadState (KCState s)
           )

runKC :: MonadError String m => (forall s. KC s a) -> m a
runKC kc = runST (runExceptT (evalStateT (runReaderT (unKC kc) env) st0))
  where
    st0 = MkKCState
      { _typeCons = Map.empty
      , _fresh    = Id.freshTVars
      }
    env = Map.empty

liftST :: ST s a -> KC s a
liftST = KC . lift . lift . lift

freshUVar :: KC s (Kind (Open s))
freshUVar = do
  v <- fresh %%= (\(v:vs) -> (v, vs))
  UVar <$> liftST (newSTRef (Free (Forget v)))

kcType :: Kind (Open s) -> Type -> KC s ()
kcType k = \case
  Ty.Var v -> do
    kv_opt <- view (at v)
    case kv_opt of
      Nothing -> throwDoc $ "unknown type variable:" <+> pretty v
      Just kv -> unify kv k
  Ty.Arr -> unify (Arrow Star (Arrow Star Star)) k
  Ty.Con Ty.MkADT{_name = con} -> do
    kcon_opt <- use (typeCons . at con)
    case kcon_opt of
      Nothing -> bug "kind checker" "unknown type constructor" (Just (show con))
      Just kcon -> unify kcon k
  Ty.App tf tp -> do
    ktp <- freshUVar
    kcType ktp tp
    kcType (Arrow ktp k) tf


kcTypDef :: [Ty.ADT (Ty.ADT Id.Con)] -> KC s ()
kcTypDef adts = do
  kinds <- for adts $ \Ty.MkADT{_name} -> do
    kind <- freshUVar
    typeCons . at _name ?= kind
    pure kind
  for_ (zip adts kinds) $ \(Ty.MkADT{_params, _constructors}, adtKind) -> do
    paramKinds <- traverse (const freshUVar) _params
    unify adtKind (foldr Arrow Star paramKinds)
    let env = Map.fromList (zip _params paramKinds)
    local (const env) $ do
      for_ _constructors $ \Ty.MkConstructor{_fields} -> do
        traverse_ (kcType Star) _fields
  traverse_ close kinds

kcVal :: Ty.Type (Ty.ADT Id.Con) Ty.Closed ->KC s ()
kcVal t = do
  env <- sequence $ Map.fromSet (const freshUVar) (Ty.vars t)
  local (const env) $ kcType Star t


kcTopLevel :: TR.TopLevel -> KC s (Maybe KC.TopLevel)
kcTopLevel = \case
  TR.TypDef w adts -> do
    here w (kcTypDef adts)
    pure Nothing
  TR.Val    w x  t -> do
    here w (kcVal t)
    pure $ Just (KC.Val w x t)
  TR.TopLet w ds   -> pure $ Just $ KC.TopLet w (fmap retagDefn ds)
  TR.TopRec w ds   -> pure $ Just $ KC.TopRec w (fmap retagDefn ds)
  TR.Asm    w x  a -> pure $ Just $ KC.Asm w x a

kcModule ::TR.Module -> KC s KC.Module
kcModule module_ = catMaybes <$> traverse kcTopLevel module_

checkModule :: MonadError String m => TR.Module -> m KC.Module
checkModule module_ = runKC (kcModule module_)


unwind :: Kind (Open s) -> KC s (Kind (Open s))
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

assertFree :: STRef s (UVar s) -> KC s ()
assertFree uref = do
  uvar <- liftST (readSTRef uref)
  case uvar of
    Free _ -> pure ()
    Link _ -> bug "kind checker" "unwinding produced link" Nothing

occursCheck :: STRef s (UVar s) -> Kind (Open s) -> KC s ()
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

unify :: Kind (Open s) -> Kind (Open s) -> KC s ()
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

close :: Kind (Open s) -> KC s ()
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
    pure $ maybeParens prec (df <+> "->" <+> dp)
  UVar uref -> do
    uvar <- readSTRef uref
    case uvar of
      Free (Forget v) -> pure (pretty v)
      Link k          -> prettyKind prec k
