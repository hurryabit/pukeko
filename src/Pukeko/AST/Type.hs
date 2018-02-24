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
  -- , weakenT
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
  , qvar2tvar
  , applyConstraints
  , prettyTypeCstr
  , prettyTUni
  , prettyQVar
  )
  where

import Pukeko.Prelude
import Pukeko.Pretty

import qualified Bound as B
import qualified Bound.Name as B
import qualified Bound.Var as B
import           Control.Lens.Indexed (FunctorWithIndex)
import           Control.Monad.Extra
import           Control.Monad.Trans.Class (lift)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Deriving
import           Data.Functor.Classes
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Extended as Map
import qualified Data.Set          as Set

import           Pukeko.AST.Name
import           Pukeko.Orphans ()

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
  | TUni (NonEmpty QVar) (B.Scope (B.Name NameTVar Int) GenType tv)

type Type = GenType (Name TVar)

pattern TArr :: GenType tv
pattern TArr = TAtm TAArr

pattern TCon :: Name TCon -> GenType tv
pattern TCon tcon = TAtm (TACon tcon)

pattern TFun :: GenType tv -> GenType tv -> GenType tv
pattern TFun tx ty = TApp (TApp TArr tx) ty

-- | A type variable which is qualified by some (potentially empty) type class
-- constraints. Used in universal quantification in types.
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

B.makeBound ''GenType

closeT :: HasCallStack => Type -> GenType Void
closeT = maybe impossible id . B.closed

mkTVarsQ :: FunctorWithIndex Int t => t QVar -> t Type
mkTVarsQ = fmap (TVar . _qvar2tvar)

mkTUni :: [QVar] -> Type -> Type
mkTUni qvs0 t0 = case qvs0 of
  []     -> t0
  qv:qvs -> TUni (qv :| qvs) (B.abstractName (env Map.!?) t0)
    where env = Map.fromList (zipWithFrom (\i (MkQVar _ v) -> (v, i)) 0 qvs0)

gatherTUni :: GenType tv -> ([QVar], B.Scope (B.Name NameTVar Int) GenType tv)
gatherTUni = \case
  TUni qvs t1 -> (toList qvs, t1)
  t0          -> ([], lift t0)

infixr 1 ~>, *~>

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

instance Eq1 GenType where
  liftEq eq t1 t2 = case (t1, t2) of
    (TVar x1, TVar x2) -> x1 `eq` x2
    (TAtm a1, TAtm a2) -> a1 == a2
    (TApp tf1 tp1, TApp tf2 tp2) -> liftEq eq tf1 tf2 && liftEq eq tp1 tp2
    (TUni xs1 tq1, TUni xs2 tq2) ->
      length xs1 == length xs2
      && and (NE.zipWith ((==) `on` _qvar2cstr) xs1 xs2)
      && liftEq eq tq1 tq2
    (TVar{}, _) -> False
    (TAtm{}, _) -> False
    (TApp{}, _) -> False
    (TUni{}, _) -> False

instance Eq v => Eq (GenType v) where (==) = eq1

instance Pretty TypeAtom where
  pretty = \case
    TAArr   -> "(->)"
    TAInt   -> "Int"
    TACon c -> pretty c

instance Pretty v => Pretty (GenType v)

instance Pretty v => PrettyPrec (GenType v) where
  prettyPrec = go pretty
    where
      go :: forall v ann. (v -> Doc ann) -> Int -> GenType v -> Doc ann
      go prettyVar prec = \case
        TVar x -> prettyVar x
        TAtm a -> pretty a
        TFun tx ty ->
          maybeParens (prec > 1) (go prettyVar 2 tx <+> "->" <+> go prettyVar 1 ty)
        TApp tf ta ->
          maybeParens (prec > 2) (go prettyVar 2 tf <+> go prettyVar 3 ta)
        TUni qvs tq ->
          prettyTUni prec qvs
          (go (B.unvar (pretty . B.name) prettyVar) 0 (B.fromScope tq))
          -- (pretty (instantiateN (fmap (TVar . _qvar2tvar) qvs) tq))

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
deriving instance Show CoercionDir
deriving instance Show Coercion

deriveShow1 ''GenType
instance Show tv => Show (GenType tv) where showsPrec = showsPrec1

makeLenses ''QVar

-- TODO: We rename types for pretty printing and only when we anticipate the
-- lexical name capture. There should be a principles approach to this in the
-- pretty printer itself.
-- renameType :: (Member NameSource effs, BaseTVar tv, Ord tv) =>
--   GenType tv -> Eff effs (GenType tv)
-- renameType t0 = do
--   let t1 = fmap Box t0
--       vs = setOf traverse t1
--       nvs = Set.fromList [Tagged [c] | c <- ['a' .. 'z']]
--             `Set.difference` Set.map (nameText . baseTVar . unBox) vs
--       -- env0 :: Map tv tv
--       env0 = Map.fromSet id vs
--       sup0 = Set.toList nvs ++ map (\n -> Tagged ('_':show n)) [1::Int ..]
--   fmap unBox <$> evalSupply sup0 (runReader env0 (go t1))
--   where
--     go :: forall tv effs. (Member NameSource effs, HasEnv tv) =>
--       GenType tv ->
--       Eff (Reader (EnvOf tv tv) : Supply (Tagged TVar String) : effs) (GenType tv)
--     go = \case
--       TVar v -> TVar <$> asks (lookupEnv v)
--       TAtm a -> pure (TAtm a)
--       TApp tf tp -> TApp <$> go tf <*> go tp
--       TUni qvs0 tq -> do
--         qvs1 <- forOf (traverse . qvar2tvar) qvs0 $ \tvar ->
--           fresh >>= mkName . Lctd (getPos tvar)
--         let env1 = imap (\i (MkQVar _ v) -> mkBound i v) qvs1
--         local' (\env0 -> extendEnv' @Int @tv env1 (fmap weakenScope env0)) $
--           TUni qvs1 <$> go tq

deriving instance Eq  TypeAtom
deriving instance Ord TypeAtom

deriveToJSON defaultOptions ''QVar
deriveToJSON defaultOptions ''TypeAtom
deriveToJSON defaultOptions ''CoercionDir
deriveToJSON defaultOptions ''Coercion
deriveToJSON1 defaultOptions ''GenType

instance ToJSON v => ToJSON (GenType v) where
  toJSON = liftToJSON toJSON toJSONList
