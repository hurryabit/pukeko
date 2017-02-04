module CoreLang.Language.LambdaLifter where

import Data.Set (Set)

import qualified Data.Set as Set

import CoreLang.Language.Syntax
import CoreLang.Language.Transform (bottomUp)

annotFreeVars :: Expr a -> Expr (Set Ident)
annotFreeVars = bottomUp f_expr f_defn f_patn . fmap (const undefined)
  where
    f_expr :: Expr (Set Ident) -> Expr (Set Ident)
    f_expr expr =
      case expr of
        Var { _ident } -> expr { _annot = Set.singleton _ident }
        Num { } -> expr { _annot = Set.empty }
        Pack { } -> expr { _annot = Set.empty }
        Ap { _fun, _arg } -> expr { _annot = annot _fun `Set.union` annot _arg }
        Let { _defns, _body } ->
          let (_patns, _exprs) = unzipDefns _defns
              _annot = (annot _body `Set.difference` Set.unions (map annot _patns))
                         `Set.union` Set.unions (map annot _exprs)
          in  expr { _annot }
        LetRec { _defns, _body } ->
          let (_patns, _exprs) = unzipDefns _defns
              _annot = (annot _body `Set.union` Set.unions (map annot _exprs))
                         `Set.difference` Set.unions (map annot _patns)
          in  expr { _annot }
        Lam { _patns, _body } ->
          expr { _annot = annot _body `Set.difference` Set.unions (map annot _patns) }
        If { _cond, _then, _else } ->
          expr { _annot = annot _cond `Set.union` annot _then `Set.union` annot _else }
        Rec { } -> error "Lambda lifiting for records not implemented"
        Sel { } -> error "Lambda lifiting for records not implemented"
    f_defn :: Defn (Set Ident) -> Defn (Set Ident)
    f_defn = id
    f_patn :: Patn (Set Ident) -> Patn (Set Ident)
    f_patn patn@MkPatn { _ident } = patn { _annot = Set.singleton _ident }
