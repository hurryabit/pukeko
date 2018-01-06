{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Language.Type
  ( Type (..)
  , TypeSchema (..)
  , (~>)
  , (*~>)
  , appN
  , appTCon
  , typeInt
  , vars
  , toSchema
  , type2tcon
  )
  where

import           Control.Lens
import           Data.Foldable (toList)
import qualified Data.Map      as Map
import           Data.Ratio    () -- for precedences in pretty printer
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set
import qualified Data.Vector.Sized as Vec

import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id
import           Pukeko.Language.AST.Scope

infixr 1 ~>, *~>

data Type tv
  = TVar tv
  | TArr
  | TCon Id.TCon
  | TApp (Type tv) (Type tv)
  deriving (Functor, Foldable, Traversable)

data TypeSchema = forall n. MkTypeSchema (Vec.Vector n Id.TVar) (Type (TFinScope n Void))

pattern TFun :: Type tv -> Type tv -> Type tv
pattern TFun tx ty = TApp (TApp TArr tx) ty

(~>) :: Type tv -> Type tv -> Type tv
(~>) = TFun

(*~>) :: [Type tv] -> Type tv -> Type tv
t_args *~> t_res = foldr (~>) t_res t_args

appN :: Type tv -> [Type tv] -> Type tv
appN = foldl TApp

appTCon :: Id.TCon -> [Type tv] -> Type tv
appTCon = appN . TCon

typeInt :: Type tv
typeInt  = TCon (Id.tcon "Int")

vars :: Ord tv => Type tv -> Set.Set tv
vars = Set.setOf traversed

toSchema :: Type Id.TVar -> TypeSchema
toSchema t = Vec.withList (toList (vars t)) $ \xs ->
  let env = ifoldMap (\i x -> Map.singleton x (mkBound i x)) xs
  in  MkTypeSchema xs (fmap (env Map.!) t)

-- * Deep traversals
type2tcon :: Traversal' (Type tv) Id.TCon
type2tcon f = \case
  TVar v     -> pure (TVar v)
  TArr       -> pure TArr
  TCon c     -> TCon <$> f c
  TApp tf tp -> TApp <$> type2tcon f tf <*> type2tcon f tp

instance IsTVar tv => Pretty (Type tv) where
  pPrintPrec lvl prec = \case
    TVar x -> pretty (baseName x)
    TArr   -> "(->)"
    TCon c -> pretty c
    TFun tx ty ->
      maybeParens (prec > 1) (pPrintPrec lvl 2 tx <+> "->" <+> pPrintPrec lvl 1 ty)
    TApp tf tx ->
      maybeParens (prec > 2) (pPrintPrec lvl 3 tf <+> pPrintPrec lvl 3 tx)

instance Pretty TypeSchema where
  pPrintPrec lvl prec (MkTypeSchema xs t) =
    "forall" <+> hsep (fmap pretty xs) <> "." <+> pPrintPrec lvl prec t

instance IsTVar tv => Show (Type tv) where
  show = prettyShow
