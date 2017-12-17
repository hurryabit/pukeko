module Pukeko.Core.Info
  ( Info (..)
  , info
  )
where

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Pukeko.Core.Syntax

data Info = MkInfo
  { _externals    :: Set Name
  , _constructors :: Set (Int, Int)
  }
  deriving (Show)

external :: Name -> Info
external ext = mempty{_externals = Set.singleton ext}

constructor :: Int -> Int -> Info
constructor tag arity = mempty{_constructors = Set.singleton (tag, arity)}

instance Monoid Info where
  mempty = MkInfo{_externals = Set.empty, _constructors = Set.empty}
  x `mappend` y = MkInfo
    { _externals    = _externals x    `Set.union` _externals y
    , _constructors = _constructors x `Set.union` _constructors y
    }

infoExpr :: Expr -> Info
infoExpr expr = case expr of
  Local{}  -> mempty
  Global{} -> mempty
  External{_name} -> external _name
  Pack{_tag, _arity} -> constructor _tag _arity
  Num{} -> mempty
  Ap{_fun, _args} -> foldMap infoExpr (_fun : _args)
  Let{_defns, _body} -> foldMap infoDefn _defns <> infoExpr _body
  Match{_expr, _altns} -> infoExpr _expr <> foldMap infoAltn _altns

infoDefn :: Defn -> Info
infoDefn MkDefn{_rhs} = infoExpr _rhs

infoAltn :: Altn -> Info
infoAltn MkAltn{_rhs} = infoExpr _rhs

infoTopDefn :: TopLevel -> Info
infoTopDefn top = case top of
  Def{_body} -> infoExpr _body
  Asm{} -> mempty

info :: Module -> Info
info = foldMap infoTopDefn