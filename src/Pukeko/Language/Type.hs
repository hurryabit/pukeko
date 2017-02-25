{-# LANGUAGE GADTs #-}
module Pukeko.Language.Type
  ( Type (..)
  , TypeVar (..)
  , Open
  , Closed
  , open
  , var
  , (~>)
  , app
  , int
  , io
  , prettyType
  )
  where

import Control.Monad.ST
import Data.Char (isLower, isUpper)
import Data.Ratio () -- for precedences in pretty printer
import Data.STRef

import Pukeko.Language.Ident
import Pukeko.Pretty hiding (int)

infixr 1 ~>

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

var :: String -> Type a
var name@(start:_)
  | isLower start = QVar (MkIdent name)
var name          = perror $ text name <+> text "is not a valid variable name"

(~>) :: Type a -> Type a -> Type a
(~>) = TFun

app :: Ident -> [Type a] -> Type a
app name@(MkIdent (start:_)) ts
  | isUpper start = TApp name ts
app name _        = perror $ pPrint name <+> text "is not a valid type constructor name"

int :: Type a
int  = app (MkIdent "Int")  []

io :: Type a -> Type a
io t = app (MkIdent "IO") [t]

open :: Type Closed -> Type (Open s)
open t =
  case t of
    QVar name  -> QVar name
    TFun tx ty -> TFun (open tx) (open ty)
    TApp c  ts -> TApp c (map open ts)

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
    QVar v -> return $ pretty v
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
