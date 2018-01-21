module Pukeko.AST.Identifier
  ( EVar
  , evar
  , op
  , main
  , isVar
  , isOp
  , stripPart
  , freshEVars
  , mangled
  , TVar
  , tvar
  , freshTVars
  , TCon
  , tcon
  , DCon
  , dcon
  , Clss
  , clss
  , annot
  )
where

import Pukeko.Prelude
import Data.Char (isLower, isUpper)

import Pukeko.Pretty
import qualified Pukeko.AST.Operator as Operator

data EVar
  = EVar{_name :: String,                  _part :: Maybe (String, Int)}
  | Op  {_sym  :: String, _name :: String, _part :: Maybe (String, Int)}
  deriving (Eq, Ord)

evar, op :: String -> EVar
evar _name@(first:_)
  | isLower first = EVar{_name, _part = Nothing}
evar _name = bugWith "invalid variable name" _name
op _sym = case Operator.mangle _sym of
  Just _name -> Op{_sym, _name, _part = Nothing}
  Nothing -> bugWith "invalid operator symbol" _sym

main :: EVar
main = evar "main"

isVar, isOp :: EVar -> Bool
isVar EVar{} = True
isVar _      = False
isOp  Op{}   = True
isOp  _      = False

stripPart :: EVar -> EVar
stripPart x = x{_part = Nothing}

freshEVars :: String -> EVar -> [EVar]
freshEVars comp var = map (\n -> var{_part = Just (comp, n)}) [1 ..]

mangled :: EVar -> String
mangled var = (_name :: EVar -> _) var ++ maybe "" (\(comp, n) -> '$':comp ++ show n) (_part var)

instance Pretty EVar where
  pPrint var = case var of
    EVar{_name, _part} -> text _name <> maybe mempty (\(comp, n) -> "$" <> text comp <> int n) _part
    Op  {_sym , _part} -> parens (text _sym <> maybe mempty (\(comp, n) -> text comp <> int n) _part)

instance Show EVar where
  show = prettyShow

data TVar
  = TVar{_name :: String}
  | IVar{_id   :: Int   }
  deriving (Eq, Ord)

tvar :: String -> TVar
tvar _name@(first:_)
  | isLower first = TVar{_name}
tvar _name = bugWith "invalid type variable name" _name

freshTVars :: [TVar]
freshTVars = map IVar [1..]

instance Pretty TVar where
  pPrint tvar = case tvar of
    TVar{_name} -> text _name
    IVar{_id}   -> "_" <> int _id

instance Show TVar where
  show = prettyShow

data XCon tag = XCon String
  deriving (Eq, Ord)

xcon :: String -> XCon tag
xcon name@(first:_)
  | isUpper first = XCon name
xcon name = bugWith "invalid constructor/clss name" name

annot :: XCon tag -> String -> XCon tag
annot (XCon name) x = XCon (name ++ "$" ++ x)

instance Pretty (XCon tag) where
  pPrint (XCon name) = text name

instance Show (XCon tag) where
  show = prettyShow

data TConTag
type TCon = XCon TConTag

tcon :: String -> TCon
tcon = xcon

data DConTag
type DCon = XCon DConTag

dcon :: String -> DCon
dcon = xcon

data ClssTag
type Clss = XCon ClssTag

clss :: String -> Clss
clss = xcon
