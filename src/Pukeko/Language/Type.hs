{-# LANGUAGE GADTs #-}
module Pukeko.Language.Type
  ( Type (..)
  , TypeVar (..)
  , Open
  , Closed
  , open
  , var
  , (~>)
  , (*~>)
  , app
  , int
  , qvars
  , prettyType
  )
  where

import Control.Monad.ST
import Data.Ratio () -- for precedences in pretty printer
import Data.Set (Set)
import Data.STRef

import qualified Data.Set as Set

import Pukeko.Pretty hiding (int)

import qualified Pukeko.Language.Ident as Ident

infixr 1 ~>, *~>

data Open s
data Closed

data TypeVar con s
  = Free { _ident :: Ident.Var, _level :: Int }
  | Link { _type  :: Type con (Open s) }

data Type con a where
  TVar :: STRef s (TypeVar con s)       -> Type con (Open s)
  QVar :: Ident.Var                        -> Type con a
  TFun :: Type con a ->  Type con a  -> Type con a
  TApp :: con           -> [Type con a] -> Type con a

var :: Ident.Var -> Type con a
var = QVar

(~>) :: Type con a -> Type con a -> Type con a
(~>) = TFun

(*~>) :: [Type con a] -> Type con a -> Type con a
t_args *~> t_res = foldr (~>) t_res t_args

app :: con -> [Type con a] -> Type con a
app = TApp

int :: Type Ident.Con Closed
int  = app (Ident.constructor "Int")  []

open :: Type con Closed -> Type con (Open s)
open t =
  case t of
    QVar name  -> QVar name
    TFun tx ty -> TFun (open tx) (open ty)
    TApp c  ts -> TApp c (map open ts)

qvars :: Type con Closed -> Set Ident.Var
qvars t = case t of
  QVar _ident -> Set.singleton _ident
  TFun t_arg t_res -> qvars t_arg `Set.union` qvars t_res
  TApp _ t_params -> Set.unions (map qvars t_params)

prettyTypeVar :: Pretty con
              => PrettyLevel -> Rational -> TypeVar con s -> ST s Doc
prettyTypeVar lvl prec tv =
  case tv of
    Free { _ident } -> return $ pretty _ident
    Link { _type }  -> brackets <$> prettyType lvl prec _type

prettyType :: Pretty con
           => PrettyLevel -> Rational -> Type con (Open s) -> ST s Doc
prettyType lvl prec t =
  case t of
    TVar tvr -> do
      tv <- readSTRef tvr
      prettyTypeVar lvl prec tv
    QVar v -> return $ braces $ pretty v
    TFun tx ty -> do
      px <- prettyType lvl 2 tx
      py <- prettyType lvl 1 ty
      return $ maybeParens (prec > 1) $ px <+> text "->" <+> py
    TApp c [] -> return $ pretty c
    TApp c ts -> do
      ps <- mapM (prettyType lvl 3) ts
      return $ maybeParens (prec > 2) $ pretty c <+> hsep ps

instance Pretty con => Pretty (Type con Closed) where
  pPrintPrec lvl prec t = runST $ prettyType lvl prec (open t)

instance Pretty con => Show (Type con Closed) where
  show = prettyShow
