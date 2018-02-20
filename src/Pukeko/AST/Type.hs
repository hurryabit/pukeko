{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Type
  ( IsType (..)
  , NoType (..)
  , TypeAtom (..)
  , GenType (..)
  , Type
  , QVar (..)
  , CoercionDir (..)
  , Coercion (..)
  , (:::) (..)
  , pattern TArr
  , pattern TCon
  , weakenT
  , closeT
  , mkTVarsQ
  , mkTUni
  , gatherTUni
  , pattern TFun
  , (~>)
  , (*~>)
  , mkTApp
  , gatherTApp
  , qvar2cstr
  , applyConstraints
  , prettyTypeCstr
  , prettyTUni
  , prettyQVar
  , renameType
  )
  where

import Pukeko.Prelude
import Pukeko.Pretty

import           Control.Lens (forOf)
import           Control.Lens.Indexed (FunctorWithIndex)
import           Control.Monad.Extra
import           Control.Monad.Freer.Supply
import           Data.Aeson.TH
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.Name
import           Pukeko.AST.Functor2
import           Pukeko.AST.Scope

infixr 1 ~>, *~>

class IsType t where
  isType :: t -> Maybe Type

data NoType = NoType

data TypeAtom
  = TAArr
  | TAInt
  | TACon (Name TCon)

data GenType tv
  = TVar tv
  | TAtm TypeAtom
  | TApp (GenType tv) (GenType tv)
  | TUni (NonEmpty QVar) (GenType (TScope Int tv))

type Type = GenType (Name TVar)

pattern TArr :: GenType tv
pattern TArr = TAtm TAArr

pattern TCon :: Name TCon -> GenType tv
pattern TCon tcon = TAtm (TACon tcon)

pattern TFun :: GenType tv -> GenType tv -> GenType tv
pattern TFun tx ty = TApp (TApp TArr tx) ty

-- | A type variable which is qualified by some (potentially empty) type class
-- constraints. Used in universal quantification in types.
--
-- The set of constraints is not considered in comparison operations.
data QVar = MkQVar
  { _qvar2cstr :: Set (Name Clss)
    -- ^ The set of type class constraints on the type variable.
  , _qvar2tvar :: Name TVar
  }

data CoercionDir = Inject | Project

data Coercion = MkCoercion
  { _coeDir  :: CoercionDir
  , _coeTCon :: Name TCon
  }

-- | A convenience data type to make pretty printing of (name, type) pairs in
-- the for "name : type" easier.
data a ::: t = a ::: t

closeT :: HasCallStack => Type -> GenType Void
closeT t0 = case traverse Left t0 of
  Left  v  -> bugWith "closeT" v
  Right t1 -> t1

strengthenT0 :: GenType (TScope Int tv) -> GenType tv
strengthenT0 = fmap strengthenScope0

weakenT :: GenType tv -> GenType (TScope i tv)
weakenT = fmap weakenScope

-- TODO: Use class 'BaseTVar' to avoid this duplication.
mkTVarsQ :: FunctorWithIndex Int t => t QVar -> t Type
mkTVarsQ = fmap (TVar . _qvar2tvar)

mkTUni :: [QVar] -> Type -> Type
mkTUni qvs = mkTUniN qvs . fmap (abstract (env Map.!?))
  where env = Map.fromList (zipWithFrom (\i (MkQVar _ v) -> (v, (i, v))) 0 qvs)

mkTUniN :: [QVar] -> GenType (TScope Int tv) -> GenType tv
mkTUniN qvs0 t0 = case qvs0 of
  []     -> strengthenT0 t0
  qv:qvs -> TUni (qv :| qvs) t0

gatherTUni :: GenType tv -> ([QVar], GenType (TScope Int tv))
gatherTUni = \case
  TUni qvs t1 -> (toList qvs, t1)
  t0          -> ([], weakenT t0)

(~>) :: GenType tv -> GenType tv -> GenType tv
(~>) = TFun

(*~>) :: Foldable t => t (GenType tv) -> GenType tv -> GenType tv
t_args *~> t_res = foldr (~>) t_res t_args

mkTApp :: (Foldable t) => GenType tv -> t (GenType tv) -> GenType tv
mkTApp = foldl TApp

gatherTApp :: GenType tv -> (GenType tv, [GenType tv])
gatherTApp = go []
  where
    go us = \case
      TApp t u -> go (u:us) t
      t        -> (t, us)

-- * Deep traversals
applyConstraints :: CanThrowHere effs =>
  [Name TVar] -> Map (Name TVar) (Set (Name Clss)) -> Eff effs [QVar]
applyConstraints binders constraints = do
  let badNames = Map.keysSet constraints `Set.difference` Set.fromList binders
  whenJust (Set.lookupMin badNames) $ \name ->
    throwHere ("constraints on unbound type variable" <:~> pretty name)
  let qualBinder binder =
        MkQVar (Map.findWithDefault mempty binder constraints) binder
  pure (map qualBinder binders)

instance IsType NoType where
  isType = const Nothing

instance IsType Type where
  isType = Just

instance (Eq tv) => Eq (GenType tv) where
  t1 == t2 = case (t1, t2) of
    (TVar x1, TVar x2) -> x1 == x2
    (TAtm a1, TAtm a2) -> a1 == a2
    (TApp tf1 tp1, TApp tf2 tp2) -> tf1 == tf2 && tp1 == tp2
    (TUni xs1 tq1, TUni xs2 tq2) ->
      length xs1 == length xs2
      && and (NE.zipWith ((==) `on` _qvar2cstr) xs1 xs2)
      && tq1 == tq2
    (TVar{}, _) -> False
    (TAtm{}, _) -> False
    (TApp{}, _) -> False
    (TUni{}, _) -> False

instance Applicative GenType where
  pure = TVar
  (<*>) = ap

instance Monad GenType where
  return = pure
  t >>= f = case t of
    TVar x -> f x
    TAtm a -> TAtm a
    TApp tf tp -> TApp (tf >>= f) (tp >>= f)
    TUni xs tq -> TUni xs (tq >>>= f)

instance Pretty TypeAtom where
  pretty = \case
    TAArr   -> "(->)"
    TAInt   -> "Int"
    TACon c -> pretty c

instance Pretty (GenType Void) where pretty = pretty @Type . vacuous
instance Pretty Type

instance PrettyPrec (GenType Void) where
  prettyPrec prec = prettyPrec @Type prec . vacuous
instance PrettyPrec Type where
  prettyPrec prec = \case
    TVar x -> pretty x
    TAtm a -> pretty a
    TFun tx ty ->
      maybeParens (prec > 1) (prettyPrec 2 tx <+> "->" <+> prettyPrec 1 ty)
    TApp tf ta ->
      maybeParens (prec > 2) (prettyPrec 2 tf <+> prettyPrec 3 ta)
    TUni qvs tq ->
      prettyTUni prec qvs (pretty (instantiateN (fmap (TVar . _qvar2tvar) qvs) tq))

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

instance (Pretty a, Pretty t) => Pretty (a ::: t) where
  pretty (x ::: t) = pretty x <+> ":" <+> pretty t

deriving instance Functor     GenType
deriving instance Foldable    GenType
deriving instance Traversable GenType

deriving instance Show QVar
deriving instance Show TypeAtom
deriving instance Show tv => Show (GenType tv)
deriving instance Show CoercionDir
deriving instance Show Coercion

newtype Boxed tv = Box{unBox :: tv}
  deriving (Eq, Ord)

instance (BaseTVar tv, Ord tv) => HasEnv (Boxed tv) where
  type EnvOf (Boxed tv) = Map (Boxed tv)
  lookupEnv v env = env Map.!  v

data TypeF typ tv
  = TVarF tv
  | TAtmF TypeAtom
  | TAppF (typ tv) (typ tv)
  | TUniF (NonEmpty QVar) (typ (TScope Int tv))

type instance Base2 GenType = TypeF

instance Functor2 TypeF
instance Traversable2 TypeF where
  traverse2 f = \case
    TVarF v      -> pure (TVarF v)
    TAtmF a      -> pure (TAtmF a)
    TAppF t1 t2  -> TAppF <$> f t1 <*> f t2
    TUniF qvs t1 -> TUniF qvs <$> f t1

instance Recursive2 GenType where
  project2 = \case
    TVar v      -> TVarF v
    TAtm a      -> TAtmF a
    TApp t1 t2  -> TAppF t1 t2
    TUni qvs t1 -> TUniF qvs t1

instance Corecursive2 GenType where
  embed2 = \case
    TVarF v      -> TVar v
    TAtmF a      -> TAtm a
    TAppF t1 t2  -> TApp t1 t2
    TUniF qvs t1 -> TUni qvs t1

makeLenses ''QVar

-- TODO: We rename types for pretty printing and only when we anticipate the
-- lexical name capture. There should be a principles approach to this in the
-- pretty printer itself.
renameType :: (Member NameSource effs, BaseTVar tv, Ord tv) =>
  GenType tv -> Eff effs (GenType tv)
renameType t0 = do
  let t1 = fmap Box t0
      vs = setOf traverse t1
      nvs = Set.fromList [Tagged [c] | c <- ['a' .. 'z']]
            `Set.difference` Set.map (nameText . baseTVar . unBox) vs
      -- env0 :: Map tv tv
      env0 = Map.fromSet id vs
      sup0 = Set.toList nvs ++ map (\n -> Tagged ('_':show n)) [1::Int ..]
  fmap unBox <$> evalSupply sup0 (runReader env0 (go t1))
  where
    go :: forall tv effs. (Member NameSource effs, HasEnv tv) =>
      GenType tv ->
      Eff (Reader (EnvOf tv tv) : Supply (Tagged TVar String) : effs) (GenType tv)
    go = \case
      TVar v -> TVar <$> asks (lookupEnv v)
      TAtm a -> pure (TAtm a)
      TApp tf tp -> TApp <$> go tf <*> go tp
      TUni qvs0 tq -> do
        qvs1 <- forOf (traverse . qvar2tvar) qvs0 $ \tvar ->
          fresh >>= mkName . Lctd (getPos tvar)
        let env1 = imap (\i (MkQVar _ v) -> mkBound i v) qvs1
        local' (\env0 -> extendEnv' @Int @tv env1 (fmap weakenScope env0)) $
          TUni qvs1 <$> go tq

deriving instance Eq  TypeAtom
deriving instance Ord TypeAtom

deriveToJSON defaultOptions ''QVar
deriveToJSON defaultOptions ''TypeAtom
deriveToJSON defaultOptions ''GenType
deriveToJSON defaultOptions ''CoercionDir
deriveToJSON defaultOptions ''Coercion
