{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.Language.Type
  ( Type (..)
  , (~>)
  , (*~>)
  , appN
  , appTCon
  , typeInt
  , vars
  , type2tcon
  )
  where

import           Control.Lens
import           Data.Ratio () -- for precedences in pretty printer
import qualified Data.Set      as Set
import qualified Data.Set.Lens as Set

import           Pukeko.Pretty
import qualified Pukeko.Language.Ident as Id

infixr 1 ~>, *~>

data Type tv
  = TVar tv
  | TArr
  | TCon Id.TCon
  | TApp (Type tv) (Type tv)
  deriving (Functor, Foldable, Traversable)

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

-- * Deep traversals
type2tcon :: Traversal' (Type tv) Id.TCon
type2tcon f = \case
  TVar v     -> pure (TVar v)
  TArr       -> pure TArr
  TCon c     -> TCon <$> f c
  TApp tf tp -> TApp <$> type2tcon f tf <*> type2tcon f tp

instance Pretty tv => Pretty (Type tv) where
  pPrintPrec lvl prec = \case
    TVar x -> pretty x
    TArr   -> "(->)"
    TCon c -> pretty c
    TFun tx ty ->
      maybeParens (prec > 1) (pPrintPrec lvl 2 tx <+> "->" <+> pPrintPrec lvl 1 ty)
    TApp tf tx ->
      maybeParens (prec > 2) (pPrintPrec lvl 3 tf <+> pPrintPrec lvl 3 tx)

instance Pretty tv => Show (Type tv) where
  show = prettyShow
