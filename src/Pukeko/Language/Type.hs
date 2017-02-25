{-# LANGUAGE Rank2Types #-}
module Pukeko.Language.Type
  ( Type
  , var
  , (~>)
  , app
  , int
  , unit
  , bool
  , pair
  , list
  , io
  , SType (..)
  , STypeVar (..)
  , toSType
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

data STypeVar s
  = Free { _ident :: Ident, _level :: Int }
  | Link { _type  :: SType s }

data SType s
  = TVar (STRef s (STypeVar s))
  | QVar Ident
  | TFun (SType s) (SType s)
  | TApp Ident [(SType s)]
  deriving (Eq)

newtype Type = MkType { toSType :: forall s. SType s }

var :: String -> Type
var name@(start:_)
  | isLower start = MkType (QVar (MkIdent name))
var name          = perror $ text name <+> text "is not a valid variable name"

(~>) :: Type -> Type -> Type
(~>) (MkType tx) (MkType ty) = MkType (TFun tx ty)

app :: Ident -> [Type] -> Type
app name@(MkIdent (start:_)) ts
  | isUpper start = MkType (TApp name $ map toSType ts)
app name _        = perror $ pPrint name <+> text "is not a valid type constructor name"

int, unit, bool :: Type
int  = app (MkIdent "Int")  []
unit = app (MkIdent "Unit") []
bool = app (MkIdent "Bool") []

pair :: Type -> Type -> Type
pair t1 t2 = app (MkIdent "Pair") [t1, t2]

list :: Type -> Type
list t = app (MkIdent "List") [t]

io :: Type -> Type
io t = app (MkIdent "IO") [t]

prettyTypeVar :: PrettyLevel -> Rational -> STypeVar s -> ST s Doc
prettyTypeVar lvl prec tv =
  case tv of
    Free { _ident } -> return $ pretty _ident
    Link { _type }  -> brackets <$> prettyType lvl prec _type

prettyType :: PrettyLevel -> Rational -> SType s -> ST s Doc
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

instance Pretty Type where
  pPrintPrec lvl prec (MkType t) =
    runST $ prettyType lvl prec t

instance Show Type where
  show = prettyShow
