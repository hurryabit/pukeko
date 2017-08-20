module Pukeko.Language.DeadCode
  ( eliminate
  )
where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

import Pukeko.Language.Syntax
import qualified Pukeko.Language.Ident as Ident

type FV = Set Ident.EVar

glDefn :: Defn _ FV -> Set Ident.EVar
glDefn MkDefn{_rhs} = glExpr _rhs

glAltn :: Altn stage FV -> Set Ident.EVar
glAltn MkAltn{_rhs} = glExpr _rhs

glExpr :: Expr stage FV -> Set Ident.EVar
glExpr expr = case expr of
  Var{_annot, _var}
    | _var `Set.member` _annot -> Set.empty
    | otherwise                -> Set.singleton _var
  Con{} -> Set.empty
  Num{} -> Set.empty
  Ap{_fun, _args} -> glExpr _fun `Set.union` Set.unions (map glExpr _args)
  Lam{_body} -> glExpr _body
  Let{_defns, _body} -> glExpr _body `Set.union` Set.unions (map glDefn _defns)
  If{_cond, _then, _else} -> Set.unions (map glExpr [_cond, _then, _else])
  Match{_expr, _altns} -> glExpr _expr `Set.union` Set.unions (map glAltn _altns)

type DC a = State (Set Ident.EVar) a

dcTopLevel :: TopLevel stage FV -> DC Bool
dcTopLevel top = case top of
  Type{} -> return True
  Val{_ident} -> gets (_ident `Set.member`)
  Def{_defns} -> do
    let (lhss, rhss) = unzipDefns _defns
    keep <- gets $ \globals -> any (`Set.member` globals) lhss
    when keep $ do
      -- TODO: Remove idents.
      let globals = Set.unions (map glExpr rhss)
      modify (globals `Set.union`)
    return keep
  Asm{_ident} -> gets (_ident `Set.member`)

eliminate :: Module _ FV -> Module _ FV
eliminate module_ = reverse $
  evalState (filterM dcTopLevel $ reverse module_) (Set.singleton Ident.main)
