module CoreLang.Language.LambdaLifter
  ( lifter
  , annotFreeVars
  )
  where

import Control.Monad.RWS
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreLang.Language.Syntax
import CoreLang.Language.Transform

import qualified CoreLang.Polymorphic.Builtins as Builtins

lifter :: Expr a -> Expr ()
lifter expr =
  let env = Set.fromList (map fst Builtins.everything)
      (_body, _defns) = runLL (liftLL (annotFreeVars expr)) env
      lifted_expr = Let { _annot = undefined, _isrec = True, _defns, _body }
  in  fmap (const ()) lifted_expr

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
        -- TODO: Remove code duplication.
        Let { _isrec = False, _defns, _body } ->
          let (_patns, _exprs) = unzipDefns _defns
              _annot = (annot _body `Set.difference` Set.unions (map annot _patns))
                         `Set.union` Set.unions (map annot _exprs)
          in  expr { _annot }
        Let { _isrec = True, _defns, _body } ->
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

data State = MkState
  { _path :: [Ident]
  , _next :: Map [Ident] Int
  }

emptyState :: State
emptyState = MkState { _path = [], _next = Map.empty }

newtype LL a = MkLL { unLL :: RWS (Set Ident) [Defn (Set Ident)] State a }
  deriving ( Functor, Applicative, Monad
           , MonadReader (Set Ident)
           , MonadWriter [Defn (Set Ident)]
           )

push :: Ident -> LL ()
push ident = 
  MkLL $ modify (\state@MkState { _path} -> state { _path = ident:_path})

pop :: LL ()
pop = MkLL $ modify (\state@MkState {_path} -> state { _path = tail _path})

fresh :: LL Ident
fresh =
  MkLL $ state $ \state@MkState{ _path, _next = old_next } ->
    let (n_opt, _next) = Map.insertLookupWithKey (const (+)) _path 1 old_next
        n = fromMaybe 0 n_opt
        ident = MkIdent $ concatMap ('$':) $ reverse $ show n : map unIdent _path
    in  (ident, state { _next })

runLL :: LL a -> Set Ident -> (a, [Defn (Set Ident)])
runLL ll env = evalRWS (unLL ll) env emptyState

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
