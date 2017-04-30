module Pukeko.Language.Ident
  ( Var
  , variable
  , operator
  , main
  , isVariable
  , isOperator
  , freshVars
  , mangled
  , Con
  , constructor
  )
where

import Data.Char (isAlpha, isLower, isUpper)

import Pukeko.Pretty
import qualified Pukeko.Language.Operator as Operator

data Var
  = Var{ _name :: String,                  _part :: Maybe Int }
  | Op { _sym  :: String, _name :: String, _part :: Maybe Int }
  deriving (Eq, Ord)

variable, operator :: String -> Var
variable _name@(first:_)
  | isLower first || not (isAlpha first) = Var{ _name, _part = Nothing }
variable _name =
  perror $ text "invalid variable name:" <+> quotes (text _name)
operator _sym = case Operator.mangle _sym of
  Just _name -> Op{ _sym, _name, _part = Nothing }
  Nothing ->
    perror $ text "invalid operator symbol:" <+> quotes (text _sym)

main :: Var
main = variable "main"

isVariable, isOperator :: Var -> Bool
isVariable Var{} = True
isVariable _     = False
isOperator Op{}  = True
isOperator _     = False

freshVars :: Var -> [Var]
freshVars var = map (\n -> var{_part = Just n}) [1 ..]

mangled :: Var -> String
mangled var = _name var ++ maybe "" (\n -> '$':show n) (_part var)

instance Pretty Var where
  pPrint var = case var of
    Var{_name, _part} -> text _name <> maybe empty (\n -> char '$' <> int n) _part
    Op {_sym , _part} -> parens (text _sym <> maybe empty int _part)

instance Show Var where
  show = prettyShow

data Con = Con String
  deriving (Eq, Ord)

constructor :: String -> Con
constructor name@(first:_)
  | isUpper first = Con name
constructor name =
  perror $ text "invalid constructor name:" <+> quotes (text name)

instance Pretty Con where
  pPrint (Con name) = text name

instance Show Con where
  show = prettyShow
