module Pukeko.AST.Identifier
  ( Named (..)
  , EVar
  , evar
  , main
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

class Named a where
  name :: a -> String

data EVar = EVar{_name :: String, _part :: Maybe (String, Int)}
  deriving (Show, Eq, Ord)

evar :: String -> EVar
evar _name@(first:_)
  | isLower first = EVar{_name, _part = Nothing}
evar _name = bugWith "invalid variable name" _name

main :: EVar
main = evar "main"

stripPart :: EVar -> EVar
stripPart x = x{_part = Nothing}

freshEVars :: String -> EVar -> [EVar]
freshEVars comp var = map (\n -> var{_part = Just (comp, n)}) [1 ..]

mangled :: EVar -> String
mangled var = (_name :: EVar -> _) var ++ maybe "" (\(comp, n) -> '$':comp ++ show n) (_part var)

instance Named EVar where
  name = render . pretty

instance Pretty EVar where
  pretty EVar{_name, _part} =
    pretty _name <> maybe mempty (\(comp, n) -> "$" <> pretty comp <> pretty n) _part

data TVar
  = TVar{_name :: String}
  | IVar{_id   :: Int   }
  deriving (Show, Eq, Ord)

tvar :: String -> TVar
tvar _name@(first:_)
  | isLower first = TVar{_name}
tvar _name = bugWith "invalid type variable name" _name

freshTVars :: [TVar]
freshTVars = map IVar [1..]

instance Named TVar where
  name = \case
    TVar{_name} -> _name
    IVar{_id}   -> '_':show _id

instance Pretty TVar where
  pretty = pretty . name

data XCon tag = XCon String
  deriving (Show, Eq, Ord)

xcon :: String -> XCon tag
xcon name@(first:_)
  | isUpper first = XCon name
xcon name = bugWith "invalid constructor/clss name" name

annot :: XCon tag -> String -> XCon tag
annot (XCon x) y = XCon (x ++ "$" ++ y)

instance Named (XCon tag) where
  name (XCon n) = n

instance Pretty (XCon tag) where
  pretty = pretty . name

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
