module Pukeko.Language.CoreCompiler
  ( compileModule
  )
where

import Control.Monad.RWS
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Pukeko.Language.ADT
import qualified Pukeko.Language.Ident  as Ident
import qualified Pukeko.Language.Syntax as L
import qualified Pukeko.Core.Syntax     as C

type FV = Set Ident.Var

type Env = Map Ident.Con Constructor
type Asm = Map Ident.Var C.Name

newtype CC a = CC{unCC :: RWS Env () Asm a}
  deriving (Functor, Applicative, Monad
           , MonadReader Env
           , MonadState Asm
           )

name :: Ident.Var -> C.Name
name = C.MkName . Ident.mangled

ccExpr :: L.Expr FV -> CC C.Expr
ccExpr expr = case expr of
  L.Var{_annot, _var}
    | _var `Set.member` _annot -> return C.Local{_name}
    | otherwise -> do
        external <- gets (_var `Map.lookup`)
        case external of
          Nothing    -> return C.Global  {_name}
          Just _name -> return C.External{_name}
    where
      _name     = name _var
  L.Con{_con} -> do
    constructor <- asks (_con `Map.lookup`)
    case constructor of
      Nothing -> error "BUG: unknown constructor in core compiler"
      Just MkConstructor{_tag, _fields} ->
        return C.Pack{_tag, _arity = length _fields}
  L.Num{_int} -> return C.Num{_int}
  L.Ap{_fun, _args} -> do
    _fun <- ccExpr _fun
    _args <- traverse ccExpr _args
    return C.Ap{_fun, _args}
  L.Lam{} -> error "BUG: lambda in core compiler"
  L.Let{_isrec, _defns, _body} -> do
    _defns <- traverse ccDefn _defns
    _body <- ccExpr _body
    return C.Let{_isrec, _defns, _body}
  L.If{} -> ccExpr (L.desugarIf expr)
  L.Match{_expr, _altns} -> do
    _expr <- ccExpr _expr
    _altns <- traverse ccAltn _altns
    return C.Match{_expr, _altns}

ccDefn :: L.Defn FV -> CC C.Defn
ccDefn L.MkDefn{_lhs = L.MkBind{_ident}, _rhs} = do
  _rhs <- ccExpr _rhs
  return C.MkDefn{_lhs = name _ident, _rhs}

ccBind :: L.Bind0 FV -> Maybe C.Name
ccBind L.MkBind{_ident} = name <$> _ident

ccAltn :: L.Altn FV -> CC C.Altn
ccAltn L.MkAltn{_binds, _rhs} = do
  _rhs <- ccExpr _rhs
  return C.MkAltn{_binds = map ccBind _binds, _rhs}

ccTopDefn :: L.Defn FV -> CC C.TopLevel
ccTopDefn L.MkDefn{_lhs = L.MkBind{_ident}, _rhs} = do
  let (_binds, _body) = case _rhs of
        L.Lam{_binds, _body} -> (_binds, _body)
        _                    -> ([]    , _rhs )
  _body <- ccExpr _body
  return C.Def{_name = name _ident, _binds = map ccBind _binds, _body}

ccTopLevel :: L.TopLevel FV -> CC [C.TopLevel]
ccTopLevel top = case top of
  L.Type{} -> return []
  L.Val{} -> return []
  L.Def{_defns} -> traverse ccTopDefn _defns
  L.Asm{_ident, _asm} -> do
    modify (Map.insert _ident (C.MkName _asm))
    return [C.Asm{_name = C.MkName _asm}]

ccModule :: L.Module FV -> CC C.Module
ccModule module_ = concat <$> mapM ccTopLevel module_

compileModule :: Map Ident.Con Constructor -> L.Module FV -> C.Module
compileModule constructors module_ =
  fst $ evalRWS (unCC (ccModule module_)) constructors Map.empty
