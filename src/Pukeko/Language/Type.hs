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
  , unit
  , bool
  , io
  , alpha
  , beta
  , qvars
  , prettyType
  )
  where

import Control.Monad.ST
import Data.Ratio () -- for precedences in pretty printer
import Data.Set (Set)
import Data.STRef

import qualified Data.Set as Set

import Pukeko.Language.Ident
import Pukeko.Pretty hiding (int)

infixr 1 ~>, *~>

data Open s
data Closed

data TypeVar s
  = Free { _ident :: Ident, _level :: Int }
  | Link { _type  :: Type s }

data Type a where
  TVar :: STRef s (TypeVar (Open s)) -> Type (Open s)
  QVar :: Ident                      -> Type a
  TFun :: Type a -> Type a           -> Type a
  TApp :: Ident  -> [Type a]         -> Type a

var :: Ident -> Type a
var ident
  | isVariable ident = QVar ident
  | otherwise        = perror $
    pretty ident <+> text "is not a valid variable name"

(~>) :: Type a -> Type a -> Type a
(~>) = TFun

(*~>) :: [Type a] -> Type a -> Type a
t_args *~> t_res = foldr (~>) t_res t_args

app :: Ident -> [Type a] -> Type a
app ident ts
  | isConstructor ident = TApp ident ts
  | otherwise           = perror $
    pPrint ident <+> text "is not a valid type constructor name"

int :: Type Closed
int  = app (MkIdent "Int")  []

bool :: Type Closed
bool = app (MkIdent "Bool") []

unit :: Type Closed
unit = app (MkIdent "Unit") []

io :: Type Closed -> Type Closed
io t = app (MkIdent "IO") [t]

alpha :: Type Closed
alpha = var (MkIdent "a")

beta :: Type Closed
beta = var (MkIdent "b")

open :: Type Closed -> Type (Open s)
open t =
  case t of
    QVar name  -> QVar name
    TFun tx ty -> TFun (open tx) (open ty)
    TApp c  ts -> TApp c (map open ts)

qvars :: Type Closed -> Set Ident
qvars t = case t of
  QVar _ident -> Set.singleton _ident
  TFun t_arg t_res -> qvars t_arg `Set.union` qvars t_res
  TApp _ t_params -> Set.unions (map qvars t_params)

prettyTypeVar :: PrettyLevel -> Rational -> TypeVar (Open s) -> ST s Doc
prettyTypeVar lvl prec tv =
  case tv of
    Free { _ident } -> return $ pretty _ident
    Link { _type }  -> brackets <$> prettyType lvl prec _type

prettyType :: PrettyLevel -> Rational -> Type (Open s) -> ST s Doc
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

instance Pretty (Type Closed) where
  pPrintPrec lvl prec t = runST $ prettyType lvl prec (open t)

instance Show (Type Closed) where
  show = prettyShow
