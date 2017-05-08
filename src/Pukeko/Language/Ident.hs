module Pukeko.Language.Ident
  ( EVar
  , evar
  , op
  , main
  , isVar
  , isOp
  , freshEVars
  , mangled
  , TVar
  , tvar
  , freshTVars
  , Con
  , constructor
  )
where

import Data.Char (isLower, isUpper)

import Pukeko.Error
import Pukeko.Pretty
import qualified Pukeko.Language.Operator as Operator

data EVar
  = EVar{_name :: String,                  _part :: Maybe Int}
  | Op  {_sym  :: String, _name :: String, _part :: Maybe Int}
  deriving (Eq, Ord)

evar, op :: String -> EVar
evar _name@(first:_)
  | isLower first = EVar{_name, _part = Nothing}
evar _name = bug "ident" "invalid variable name" (Just _name)
op _sym = case Operator.mangle _sym of
  Just _name -> Op{_sym, _name, _part = Nothing}
  Nothing -> bug "ident" "invalid operator symbol" (Just _sym)

main :: EVar
main = evar "main"

isVar, isOp :: EVar -> Bool
isVar EVar{} = True
isVar _      = False
isOp  Op{}   = True
isOp  _      = False

freshEVars :: EVar -> [EVar]
freshEVars var = map (\n -> var{_part = Just n}) [1 ..]

mangled :: EVar -> String
mangled var = (_name :: EVar -> _) var ++ maybe "" (\n -> '$':show n) (_part var)

instance Pretty EVar where
  pPrint var = case var of
    EVar{_name, _part} -> text _name <> maybe empty (\n -> "$" <> int n) _part
    Op  {_sym , _part} -> parens (text _sym <> maybe empty int _part)

instance Show EVar where
  show = prettyShow

data TVar
  = TVar{_name :: String}
  | IVar{_id   :: Int   }
  deriving (Eq, Ord)

tvar :: String -> TVar
tvar _name@(first:_)
  | isLower first = TVar{_name}
tvar _name = bug "ident" "invalid type variable name" (Just _name)

freshTVars :: [TVar]
freshTVars = map IVar [1..]

instance Pretty TVar where
  pPrint tvar = case tvar of
    TVar{_name} -> text _name
    IVar{_id}   -> "_" <> int _id

instance Show TVar where
  show = prettyShow

data Con = Con String
  deriving (Eq, Ord)

constructor :: String -> Con
constructor name@(first:_)
  | isUpper first = Con name
constructor name = bug "ident" "invalid constructor name" (Just name)

instance Pretty Con where
  pPrint (Con name) = text name

instance Show Con where
  show = prettyShow
