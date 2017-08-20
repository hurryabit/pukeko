module Pukeko.Language.CoreCompiler
  ( compileModule
  )
where

import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Pukeko.Error
import Pukeko.Language.Type (Constructor (..))
import qualified Pukeko.Language.Ident  as Ident
import qualified Pukeko.Language.Syntax as L
import qualified Pukeko.Core.Syntax     as C

type FV = Set Ident.EVar

type CCState = Map Ident.EVar C.Name

newtype CC a = CC{unCC :: State CCState a}
  deriving (Functor, Applicative, Monad
           , MonadState CCState
           )

name :: Ident.EVar -> C.Name
name = C.MkName . Ident.mangled

ccExpr :: L.Expr L.StageTR FV -> CC C.Expr
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
  L.Con{_con = MkConstructor{_tag, _fields}} ->
    return C.Pack{_tag, _arity = length _fields}
  L.Num{_int} -> return C.Num{_int}
  L.Ap{_fun, _args} -> do
    _fun <- ccExpr _fun
    _args <- traverse ccExpr _args
    return C.Ap{_fun, _args}
  L.Lam{} -> bug "core compiler" "unlifted lambda" Nothing
  L.Let{_isrec, _defns, _body} -> do
    _defns <- traverse ccDefn _defns
    _body <- ccExpr _body
    return C.Let{_isrec, _defns, _body}
  L.If{} -> ccExpr (L.desugarIf expr)
  L.Match{_expr, _altns} -> do
    _expr <- ccExpr _expr
    _altns <- traverse ccAltn _altns
    return C.Match{_expr, _altns}

ccDefn :: L.Defn L.StageTR FV -> CC C.Defn
ccDefn L.MkDefn{_lhs, _rhs} = do
  _rhs <- ccExpr _rhs
  return C.MkDefn{_lhs = name _lhs, _rhs}

ccPatn :: L.Patn1 L.StageTR FV -> Maybe C.Name
ccPatn patn = case patn of
  L.Wild{}       -> Nothing
  L.Bind{_ident} -> Just (name _ident)

ccAltn :: L.Altn L.StageTR FV -> CC C.Altn
ccAltn L.MkAltn{_patns, _rhs} = do
  _rhs <- ccExpr _rhs
  return C.MkAltn{_binds = map ccPatn _patns, _rhs}

ccTopDefn :: L.Defn L.StageTR FV -> CC C.TopLevel
ccTopDefn L.MkDefn{_lhs, _rhs} = do
  let (_patns, _body) = case _rhs of
        L.Lam{_patns, _body} -> (_patns, _body)
        _                    -> ([]    , _rhs )
  _body <- ccExpr _body
  return C.Def{_name = name _lhs, _binds = map ccPatn _patns, _body}

ccTopLevel :: L.TopLevel L.StageTR FV -> CC [C.TopLevel]
ccTopLevel top = case top of
  L.Type{} -> return []
  L.Val{} -> return []
  L.Def{_defns} -> traverse ccTopDefn _defns
  L.Asm{_ident, _asm} -> do
    modify (Map.insert _ident (C.MkName _asm))
    return [C.Asm{_name = C.MkName _asm}]

ccModule :: L.Module L.StageTR FV -> CC C.Module
ccModule module_ = concat <$> mapM ccTopLevel module_

compileModule :: L.Module L.StageTR FV -> C.Module
compileModule module_ = evalState (unCC (ccModule module_)) Map.empty
