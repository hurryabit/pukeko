{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.AST.Type
  ( IsType (..)
  , NoType (..)
  , Type (..)
  , mkTUni
  , pattern TFun
  , (~>)
  , (*~>)
  , mkTApp
  , gatherTApp
  , typeInt
  , vars
  , toPrenex
  , type2tcon
  , Void
  , absurd
  )
  where

import           Control.Lens  hiding ((.=))
import           Control.Monad
import           Data.CallStack
import           Data.Finite   (absurd0)
import           Data.Foldable (toList)
import qualified Data.Map      as Map
import           Data.Proxy
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set
import           Data.Type.Equality ((:~:) (Refl))
import qualified Data.Vector.Sized as Vec
import           GHC.TypeLits

import           Pukeko.Error
import           Pukeko.Pretty
import           Pukeko.AST.Pos
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
    TUni (Vec.Vector n Id.TVar) (Type (TFinScope n tv))

pattern TFun :: Type tv -> Type tv -> Type tv
pattern TFun tx ty = TApp (TApp TArr tx) ty

mkTUni ::
  forall n tv. KnownNat n => Vec.Vector n Id.TVar -> Type (TFinScope n tv) -> Type tv
mkTUni xs t =
  case sameNat (Proxy @n) (Proxy @0) of
    Nothing   -> TUni xs t
    Just Refl -> fmap (strengthenWith absurd0) t

(~>) :: Type tv -> Type tv -> Type tv
(~>) = TFun

(*~>) :: Foldable t => t (Type tv) -> Type tv -> Type tv
t_args *~> t_res = foldr (~>) t_res t_args

mkTApp :: Type tv -> [Type tv] -> Type tv
mkTApp = foldl TApp

gatherTApp :: Type tv -> (Type tv, [Type tv])
gatherTApp = go []
  where
    go us = \case
      TApp t u -> go (u:us) t
      t        -> (t, us)

typeInt :: Type tv
typeInt  = TCon (Id.tcon "Int")

vars :: Ord tv => Type tv -> Set.Set tv
vars = Set.setOf traversed

toPrenex :: HasCallStack => Type Id.TVar -> Type Void
toPrenex t =
  Vec.withList (toList (vars t)) $ \xs ->
    let env = ifoldMap (\i x -> Map.singleton x (mkBound i x)) xs
    in  mkTUni xs (fmap (\x -> Map.findWithDefault (bugWith "toPrexex" x) x env) t)

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
        Just Refl -> tq1 == tq2
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
    TUni vs tq ->
      maybeParens lvl (prec > 0) ("∀" <> hsepMap pretty vs <> "." <+> pPrintPrec lvl 0 tq)

deriving instance Functor     Type
deriving instance Foldable    Type
deriving instance Traversable Type

deriving instance Functor     NoType
deriving instance Foldable    NoType
deriving instance Traversable NoType

instance FunctorWithIndex     Pos Type where
instance FoldableWithIndex    Pos Type where
instance TraversableWithIndex Pos Type where
  itraverse f = \case
    TVar x -> TVar <$> f noPos x
    TArr   -> pure TArr
    TCon c -> pure (TCon c)
    TApp tf tp -> TApp <$> itraverse f tf <*> itraverse f tp
    TUni vs tq -> TUni vs <$> itraverse (traverse . f) tq

instance FunctorWithIndex     Pos NoType where
instance FoldableWithIndex    Pos NoType where
instance TraversableWithIndex Pos NoType where
  itraverse _ NoType = pure NoType

deriving instance Show tv => Show (Type tv)
