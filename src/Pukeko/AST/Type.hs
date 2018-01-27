{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Type
  ( IsType (..)
  , NoType (..)
  , Type (..)
  , QVar (..)
  , mkTVars
  , mkTVarsQ
  , mkTUni
  , withTUni
  , pattern TFun
  , (~>)
  , (*~>)
  , mkTApp
  , gatherTApp
  , typeInt
  , vars
  , qvar2cstr
  , qvar2tvar
  , type2tcon
  , prettyTypeCstr
  , prettyTUni
  , prettyQVar
  , renameType
  )
  where

import Pukeko.Prelude

import           Control.Lens ()
import           Control.Monad.Freer.Supply
import           Data.Finite       (absurd0)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.Pretty
import qualified Pukeko.AST.Identifier as Id
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
  | forall n. KnownNat n =>
    TUni (Vector n QVar) (Type (TFinScope n tv))

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

-- instance Eq QVar where
--   (==) = (==) `on` _qvar2tvar

-- instance Ord QVar where
--   compare = compare `on` _qvar2tvar

mkTVars :: Vector n Id.TVar -> Vector n (Type (TFinScope n tv))
mkTVars = imap (\i b -> TVar (mkBound i b))

-- TODO: Use class 'BaseTVar' to avoid this duplication.
mkTVarsQ :: Vector n QVar -> Vector n (Type (TFinScope n tv))
mkTVarsQ = mkTVars . fmap _qvar2tvar

mkTUni :: forall n tv. KnownNat n => Vector n QVar -> Type (TFinScope n tv) -> Type tv
mkTUni xs t =
  case sameNat (Proxy @n) (Proxy @0) of
    Nothing   -> TUni xs t
    Just Refl -> fmap (strengthenWith absurd0) t

withTUni ::
  Type tv -> (forall m. (KnownNat m) => Vector m QVar -> Type (TFinScope m tv) -> a) -> a
withTUni t0 k = case t0 of
  TUni qvs t1 -> k qvs t1
  _           -> k Vec.empty (fmap weaken t0)

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

-- * Deep traversals
type2tcon :: Traversal' (Type tv) Id.TCon
type2tcon f = \case
  TVar v     -> pure (TVar v)
  TArr       -> pure TArr
  TCon c     -> TCon <$> f c
  TApp tf tp -> TApp <$> type2tcon f tf <*> type2tcon f tp
  TUni xs tq -> TUni xs <$> type2tcon f tq

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
      case sameNat (Vec.plength xs1) (Vec.plength xs2) of
        Nothing   -> False
        Just Refl -> and (Vec.zipWith ((==) `on` _qvar2cstr) xs1 xs2) && tq1 == tq2
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

instance BaseTVar tv => Pretty (Type tv) where
  pPrintPrec lvl prec = \case
    TVar x -> pretty (baseTVar x)
    TArr   -> "(->)"
    TCon c -> pretty c
    TFun tx ty ->
      maybeParens lvl (prec > 1) (pPrintPrec lvl 2 tx <+> "->" <+> pPrintPrec lvl 1 ty)
    TApp tf tx ->
      maybeParens lvl (prec > 2) (pPrintPrec lvl 2 tf <+> pPrintPrec lvl 3 tx)
    TUni qvs tq -> prettyTUni lvl prec qvs (pPrintPrec lvl 0 tq)

prettyTypeCstr :: Foldable t => t QVar -> Doc
prettyTypeCstr qvs
  | null qs   = mempty
  | otherwise = parens (hsep (punctuate "," qs)) <+> "=>"
  where
    qs = [ pretty c <+> pretty v | MkQVar q v <- toList qvs, c <- toList q ]

prettyTUni :: Foldable t => PrettyLevel -> Rational -> t QVar -> Doc -> Doc
prettyTUni lvl prec qvs tq =
  maybeParens lvl (prec > 0)
  ("∀" <> hsepMap (pretty . _qvar2tvar) qvs <> "." <+> prettyTypeCstr qvs <+> tq)

prettyQVar :: QVar -> Doc
prettyQVar (MkQVar q v)
  | null q    = pretty v
  | otherwise = parens (pretty v <+> "|" <+> hsepMap pretty q)

makeLenses ''QVar

deriving instance Functor     Type
deriving instance Foldable    Type
deriving instance Traversable Type

deriving instance Functor     NoType
deriving instance Foldable    NoType
deriving instance Traversable NoType

deriving instance Show QVar
deriving instance Show tv => Show (Type tv)

newtype Boxed tv = Box{unBox :: tv}
  deriving (Eq, Ord)

instance (BaseTVar tv, Ord tv) => HasEnv (Boxed tv) where
  type EnvOf (Boxed tv) = Map (Boxed tv)
  lookupEnv v@(Box v0) = Map.findWithDefault (bugWith "lookupEnv" (baseTVar v0)) v

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
      TUni (qvs0 :: Vector n QVar) tq -> do
        qvs1 <- traverse (qvar2tvar (const fresh)) qvs0
        let env1 = imap (\i (MkQVar _ v) -> mkBound i v) qvs1
        local' (\env0 -> extendEnv @(Finite n) @tv env1 (fmap weaken env0)) $
          TUni qvs1 <$> go tq
