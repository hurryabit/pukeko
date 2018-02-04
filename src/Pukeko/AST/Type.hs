{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Type
  ( IsType (..)
  , NoType (..)
  , Type (..)
  , QVar (..)
  , CoercionDir (..)
  , Coercion (..)
  , weakenT
  , strengthenT0
  , mkTVars
  , mkTVarsQ
  , mkTUni
  , gatherTUni
  , pattern TFun
  , (~>)
  , (*~>)
  , mkTApp
  , gatherTApp
  , typeInt
  , vars
  , qvar2cstr
  , qvar2tvar
  , coercion2type
  , type2tcon
  , type2tcon_
  , prettyTypeCstr
  , prettyTUni
  , prettyQVar
  , renameType
  )
  where

import Pukeko.Prelude

import           Control.Lens.Indexed (FunctorWithIndex)
import           Control.Monad.Freer.Supply
import qualified Data.List.NonEmpty as NE
import qualified Data.Map          as Map
import qualified Data.Set          as Set

import           Pukeko.Pretty
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Functor2
import           Pukeko.AST.Scope

infixr 1 ~>, *~>

class IsType t where
  isType :: t tv -> Maybe (Type tv)

data NoType tv = NoType

data Type tv
  = TVar tv
  | TArr
  | TCon Id.TCon
  | TApp (Type tv) (Type tv)
  | TUni (NonEmpty QVar) (Type (TScope Int tv))

pattern TFun :: Type tv -> Type tv -> Type tv
pattern TFun tx ty = TApp (TApp TArr tx) ty

-- | A type variable which is qualified by some (potentially empty) type class
-- constraints. Used in universal quantification in types.
--
-- The set of constraints is not considered in comparison operations.
data QVar = MkQVar
  { _qvar2cstr :: Set Id.Clss
    -- ^ The set of type class constraints on the type variable.
  , _qvar2tvar :: Id.TVar
  }

data CoercionDir = Inject | Project

data Coercion ty = MkCoercion
  { _coeDir  :: CoercionDir
  , _coeTCon :: Id.TCon
  , _coeFrom :: ty
  , _coeTo   :: ty
  }

strengthenT0 :: Type (TScope Int tv) -> Type tv
strengthenT0 = fmap strengthenScope0

weakenT :: Type tv -> Type (TScope i tv)
weakenT = fmap weakenScope

mkTVars :: (FunctorWithIndex Int t) => t Id.TVar -> t (Type (TScope Int tv))
mkTVars = imap (\i b -> TVar (mkBound i b))

-- TODO: Use class 'BaseTVar' to avoid this duplication.
mkTVarsQ :: FunctorWithIndex Int t => t QVar -> t (Type (TScope Int tv))
mkTVarsQ = mkTVars . fmap _qvar2tvar

mkTUni :: [QVar] -> Type (TScope Int tv) -> Type tv
mkTUni qvs0 t0 = case qvs0 of
  []     -> strengthenT0 t0
  qv:qvs -> TUni (qv :| qvs) t0

gatherTUni :: Type tv -> ([QVar], Type (TScope Int tv))
gatherTUni = \case
  TUni qvs t1 -> (toList qvs, t1)
  t0          -> ([], weakenT t0)

(~>) :: Type tv -> Type tv -> Type tv
(~>) = TFun

(*~>) :: Foldable t => t (Type tv) -> Type tv -> Type tv
t_args *~> t_res = foldr (~>) t_res t_args

mkTApp :: (Foldable t) => Type tv -> t (Type tv) -> Type tv
mkTApp = foldl TApp

gatherTApp :: Type tv -> (Type tv, [Type tv])
gatherTApp = go []
  where
    go us = \case
      TApp t u -> go (u:us) t
      t        -> (t, us)

typeInt :: Type tv
typeInt  = TCon (Id.tcon "Int")

vars :: Ord tv => Type tv -> Set tv
vars = setOf traverse

coercion2type :: Traversal (Coercion ty1) (Coercion ty2) ty1 ty2
coercion2type f (MkCoercion dir tcon from to) = MkCoercion dir tcon <$> f from <*> f to

-- * Deep traversals
type2tcon :: forall m tv. Monad m => (Id.TCon -> m Id.TCon) -> Type tv -> m (Type tv)
type2tcon f = cataM2 (fmap embed2 . step)
  where
    step :: TypeF Type tv' -> m (TypeF Type tv')
    step = \case
      TConF c -> TConF <$> f c
      x       -> pure x

type2tcon_ :: forall m tv. Monad m => (Id.TCon -> m ()) -> Type tv -> m ()
type2tcon_ f = fmap getConst . cataM2 (fmap Const . step)
  where
    step :: TypeF _ _ -> m ()
    step = \case
      TConF c -> f c
      _       -> pure ()

instance IsType NoType where
  isType = const Nothing

instance IsType Type where
  isType = Just

instance (Eq tv) => Eq (Type tv) where
  t1 == t2 = case (t1, t2) of
    (TVar x1, TVar x2) -> x1 == x2
    (TArr   , TArr   ) -> True
    (TCon c1, TCon c2) -> c1 == c2
    (TApp tf1 tp1, TApp tf2 tp2) -> tf1 == tf2 && tp1 == tp2
    (TUni xs1 tq1, TUni xs2 tq2) ->
      length xs1 == length xs2
      && and (NE.zipWith ((==) `on` _qvar2cstr) xs1 xs2)
      && tq1 == tq2
    (TVar{}, _) -> False
    (TArr{}, _) -> False
    (TCon{}, _) -> False
    (TApp{}, _) -> False
    (TUni{}, _) -> False

instance Applicative Type where
  pure = TVar
  (<*>) = ap

instance Monad Type where
  return = pure
  t >>= f = case t of
    TVar x -> f x
    TArr   -> TArr
    TCon c -> TCon c
    TApp tf tp -> TApp (tf >>= f) (tp >>= f)
    TUni xs tq -> TUni xs (tq >>>= f)

instance BaseTVar tv => Pretty (Type tv)

instance BaseTVar tv => PrettyPrec (Type tv) where
  prettyPrec prec = \case
    TVar x -> pretty (baseTVar x)
    TArr   -> "(->)"
    TCon c -> pretty c
    TFun tx ty ->
      maybeParens (prec > 1) (prettyPrec 2 tx <+> "->" <+> prettyPrec 1 ty)
    TApp tf tx ->
      maybeParens (prec > 2) (prettyPrec 2 tf <+> prettyPrec 3 tx)
    TUni qvs tq -> prettyTUni prec qvs (pretty tq)

prettyTypeCstr :: Foldable t => t QVar -> Doc ann
prettyTypeCstr qvs
  | null qs   = mempty
  | otherwise = parens (hsep (punctuate "," qs)) <+> "=>"
  where
    qs = [ pretty c <+> pretty v | MkQVar q v <- toList qvs, c <- toList q ]

prettyTUni :: Foldable t => Int -> t QVar -> Doc ann -> Doc ann
prettyTUni prec qvs tq =
  maybeParens (prec > 0)
  ("∀" <> hsepMap (pretty . _qvar2tvar) qvs <> "." <+> prettyTypeCstr qvs <+> tq)

prettyQVar :: QVar -> Doc ann
prettyQVar (MkQVar q v)
  | null q    = pretty v
  | otherwise = parens (pretty v <+> "|" <+> hsepMap pretty q)

deriving instance Functor     Type
deriving instance Foldable    Type
deriving instance Traversable Type

deriving instance Functor     NoType
deriving instance Foldable    NoType
deriving instance Traversable NoType

deriving instance Show QVar
deriving instance Show tv => Show (Type tv)
deriving instance Show CoercionDir
deriving instance Show ty => Show (Coercion ty)

newtype Boxed tv = Box{unBox :: tv}
  deriving (Eq, Ord)

instance (BaseTVar tv, Ord tv) => HasEnv (Boxed tv) where
  type EnvOf (Boxed tv) = Map (Boxed tv)
  lookupEnv v@(Box v0) = Map.findWithDefault (bugWith "lookupEnv" (baseTVar v0)) v

data TypeF typ tv
  = TVarF tv
  | TArrF
  | TConF Id.TCon
  | TAppF (typ tv) (typ tv)
  | TUniF (NonEmpty QVar) (typ (TScope Int tv))

type instance Base2 Type = TypeF

instance Functor2 TypeF
instance Traversable2 TypeF where
  traverse2 f = \case
    TVarF v      -> pure (TVarF v)
    TArrF        -> pure TArrF
    TConF c      -> pure (TConF c)
    TAppF t1 t2  -> TAppF <$> f t1 <*> f t2
    TUniF qvs t1 -> TUniF qvs <$> f t1

instance Recursive2 Type where
  project2 = \case
    TVar v      -> TVarF v
    TArr        -> TArrF
    TCon c      -> TConF c
    TApp t1 t2  -> TAppF t1 t2
    TUni qvs t1 -> TUniF qvs t1

instance Corecursive2 Type where
  embed2 = \case
    TVarF v      -> TVar v
    TArrF        -> TArr
    TConF c      -> TCon c
    TAppF t1 t2  -> TApp t1 t2
    TUniF qvs t1 -> TUni qvs t1

makeLenses ''QVar

renameType :: (BaseTVar tv, Ord tv) => Type tv -> Type tv
renameType t0 =
  let t1 = fmap Box t0
      vs = setOf traverse t1
      nvs = Set.fromList [Id.tvar [c] | c <- ['a' .. 'z']]
            `Set.difference` Set.map (baseTVar . unBox) vs
      -- env0 :: Map tv tv
      env0 = Map.fromSet id vs
      sup0 = Set.toList nvs ++ Id.freshTVars
  in fmap unBox (run (evalSupply sup0 (runReader env0 (go t1))))
  where
    go ::
      forall tv. (HasEnv tv) =>
      Type tv -> Eff [Reader (EnvOf tv tv), Supply Id.TVar] (Type tv)
    go = \case
      TVar v -> TVar <$> asks (lookupEnv v)
      TArr -> pure TArr
      TCon c -> pure (TCon c)
      TApp tf tp -> TApp <$> go tf <*> go tp
      TUni qvs0 tq -> do
        qvs1 <- traverse (qvar2tvar (const fresh)) qvs0
        let env1 = imap (\i (MkQVar _ v) -> mkBound i v) qvs1
        local' (\env0 -> extendEnv' @Int @tv env1 (fmap weakenScope env0)) $
          TUni qvs1 <$> go tq
