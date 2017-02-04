module CoreLang.Language.LambdaLifter
  ( lifter
  )
  where

import Control.Monad.Supply
import Control.Monad.Writer
import Data.Set (Set)

import qualified Data.Set as Set

import CoreLang.Language.Syntax
import CoreLang.Language.Transform

import qualified CoreLang.Polymorphic.Builtins as Builtins

lifter :: Expr a -> Expr ()
lifter expr =
  let (_body, _defns) = runLL $ liftLL (annotFreeVars expr)
      _expr = LetRec { _annot = undefined, _defns, _body }
  in  fmap (const ()) _expr

lookupFreeVars :: Ident -> Set Ident
lookupFreeVars ident =
  case lookup ident Builtins.everything of
    Nothing -> Set.singleton ident
    Just _  -> Set.empty

annotFreeVars :: Expr a -> Expr (Set Ident)
annotFreeVars = bottomUp f_expr f_defn f_patn . fmap (const undefined)
  where
    f_expr :: Expr (Set Ident) -> Expr (Set Ident)
    f_expr expr =
      case expr of
        Var { _ident } -> expr { _annot = lookupFreeVars _ident }
        Num  { } -> expr { _annot = Set.empty }
        Pack { } -> expr { _annot = Set.empty }
        Ap { _fun, _arg } -> expr { _annot = annot _fun `Set.union` annot _arg }
        ApOp { _op, _arg1, _arg2 } ->
          let _annot = lookupFreeVars _op `Set.union` annot _arg1 `Set.union` annot _arg2
          in  expr { _annot }
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

newtype LL a = MkLL { unLL :: WriterT [Defn (Set Ident)] (Supply Ident) a }
  deriving ( Functor, Applicative, Monad
           , MonadWriter [Defn (Set Ident)]
           , MonadSupply Ident
           )

runLL :: LL a -> (a, [Defn (Set Ident)])
runLL ll = evalSupply (runWriterT (unLL ll)) (map (\n -> MkIdent $ '$':show n) [1::Int ..])

liftLL :: Expr (Set Ident) -> LL (Expr (Set Ident))
liftLL = bottomUpM f_expr return return
  where
    f_expr expr = 
      case expr of
        Lam { _annot, _patns, _body } -> do
          lifted_ident <- fresh
          let mkPatn _ident = MkPatn { _annot = Set.singleton _ident, _ident, _type = Nothing }
              free_vars = Set.toList _annot
              lifted_expr = Lam { _annot = Set.empty, _patns = map mkPatn free_vars ++ _patns, _body }
              defn = MkDefn { _patn = mkPatn lifted_ident, _expr = lifted_expr }
              lifted_call = foldl1 (Ap _annot) $ map (Var _annot) (lifted_ident : free_vars)
          tell [defn]
          return lifted_call
        _ -> return expr
