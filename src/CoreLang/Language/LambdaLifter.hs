module CoreLang.Language.LambdaLifter
  ( liftExpr
  )
  where

import Control.Monad.Reader
import Control.Monad.RWS
import Data.List (partition)
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import CoreLang.Language.Rewrite
import CoreLang.Language.Syntax

liftExpr :: [Ident] -> Expr a -> FvExpr
liftExpr globals_list expr =
  let globals = Set.fromList globals_list
      fv_expr = fvExpr globals expr
      (_body, _defns) = runLL (llExpr fv_expr) globals
  in  fvAdjust $ Let { _annot = undefined, _isrec = True, _defns, _body }

data Scope = Local | Global
  deriving (Show)

mkVar :: Scope -> Ident -> FvExpr
mkVar scope _ident =
  case scope of
    Local  -> Var { _annot = Set.singleton _ident, _ident }
    Global -> Var { _annot = Set.empty           , _ident }

type FvExpr = Expr (Set Ident)
type FvDefn = Defn (Set Ident)
type FvPatn = Patn (Set Ident)

fvExpr :: Set Ident -> Expr a -> FvExpr
fvExpr globals expr =
  let rewrite = emptyRewrite { rewrite_expr, rewrite_patn }
      fv_expr = fmap (const undefined) expr
  in  runReader (runRewrite rewrite fv_expr) globals
  where
    localize idents = local (`Set.difference` Set.fromList idents)
    rewrite_expr descend old_expr = do
      post_expr <-
        case old_expr of
          Var { _ident } -> do
            is_global <- asks (_ident `Set.member`)
            let scope = if is_global then Global else Local
            return $ mkVar scope _ident
          Lam { _patns, _body } -> do
            let (bound_idents, _) = unzipPatns _patns
            localize bound_idents (descend old_expr)
          Let { _isrec = True, _defns, _body } -> do
            let (bound_idents, _, _) = unzipDefns3 _defns
            localize bound_idents (descend old_expr)
          Let { _isrec = False, _defns, _body = old_body } -> do
            -- This is kind of hacky. Be careful when refactoring.
            post_expr' <- descend old_expr
            let (bound_idents, _, _) = unzipDefns3 _defns
            post_body <- localize bound_idents (rewrite_expr descend old_body)
            return $ post_expr' { _body = post_body }
          -- The remaining cases are boilerplate.
          Num  {} -> descend old_expr
          Pack {} -> descend old_expr
          Ap   {} -> descend old_expr
          ApOp {} -> descend old_expr
          If   {} -> descend old_expr
          Rec  {} -> descend old_expr
          Sel  {} -> descend old_expr
      return $ fvAdjust post_expr
    rewrite_patn _ patn@MkPatn{ _ident } =
      return (patn { _annot = Set.singleton _ident } :: FvPatn)

fvAdjust :: FvExpr -> FvExpr
fvAdjust expr =
  case expr of
    Var { } -> expr
    Num  { } -> expr { _annot = Set.empty }
    Pack { } -> expr { _annot = Set.empty }
    Ap { _fun, _arg } -> expr { _annot = annot _fun `Set.union` annot _arg }
    ApOp { _op, _arg1, _arg2 } ->
      let _annot = Set.unions [annot _arg1, annot _arg2]
      in  expr { _annot }
    Let { _isrec, _defns, _body } ->
      let (patns, rhss) = unzipDefns _defns
          fv_patns = Set.unions (map annot patns)
          fv_rhss  = Set.unions (map annot rhss )
          fv_body  = annot _body
          _annot
            | _isrec    = (fv_rhss `Set.union`  fv_body) `Set.difference` fv_patns
            | otherwise =  fv_rhss `Set.union` (fv_body  `Set.difference` fv_patns)
      in  expr { _annot }
    Lam { _patns, _body } ->
      expr { _annot = annot _body `Set.difference` Set.unions (map annot _patns) }
    If { _cond, _then, _else } ->
      expr { _annot = annot _cond `Set.union` annot _then `Set.union` annot _else }
    Rec { } -> error "Lambda lifiting for records not implemented"
    Sel { } -> error "Lambda lifiting for records not implemented"

fvPatn :: Patn a -> FvPatn
fvPatn patn@MkPatn { _ident } = patn { _annot = Set.singleton _ident }

data State = MkState { _used :: Map Ident Int }

emptyState :: Set Ident -> State
emptyState globals = MkState { _used = Map.fromSet (const 1) globals }

newtype LL a = MkLL { unLL :: RWS Ident [FvDefn] State a }
  deriving ( Functor, Applicative, Monad
           , MonadWriter [FvDefn]
           )

data HowFresh = Nice | Lambda

fresh :: HowFresh -> LL Ident
fresh how = MkLL $ do
  context <- ask
  let mkIdent n = MkIdent $ unIdent context ++ '$':show n
  state $ \state@MkState{ _used = old_used } ->
    let (n_opt, new_used) = Map.insertLookupWithKey (const (+)) context 1 old_used
        ident =
          case (n_opt, how) of
            (Nothing, Nice  ) -> context
            (Nothing, Lambda) -> mkIdent 0
            (Just n , _     ) -> mkIdent n
    in  (ident, state { _used = new_used })

within :: Ident -> LL a -> LL a
within ident ll = MkLL $ local (const ident) (unLL ll)

runLL :: LL a -> Set Ident -> (a, [FvDefn])
runLL ll globals  = evalRWS (unLL ll) (MkIdent "main") (emptyState globals)

mkPatn :: Ident -> FvPatn
mkPatn _ident = fvPatn $ MkPatn { _annot = undefined, _ident, _type = Nothing }

mkDefn :: Ident -> FvExpr -> FvDefn
mkDefn _ident _expr = MkDefn { _patn = mkPatn _ident, _expr }


llLamAs :: Ident -> Set Ident -> [FvPatn] -> FvExpr -> LL FvExpr
llLamAs global_ident old_annot old_patns old_body = do
  new_body <- llExpr old_body
  let fv_lambda = Set.toList old_annot
      new_patns = map mkPatn fv_lambda ++ old_patns
      lifted_lambda = fvAdjust $
        Lam { _annot = undefined, _patns = new_patns, _body = new_body }
  tell [mkDefn global_ident lifted_lambda]
  let ap _fun ident =
        fvAdjust $ Ap { _annot = undefined, _fun, _arg = mkVar Local ident }
  return $ foldl ap (mkVar Global global_ident) fv_lambda

llExprAs :: Ident -> FvExpr -> LL ()
llExprAs global_ident expr =
  case expr of
    Lam { _annot, _patns, _body } -> do
      _ <- llLamAs global_ident _annot _patns _body
      return ()
    _ -> do
      lifted_expr <- llExpr expr
      tell [mkDefn global_ident lifted_expr]

llExpr :: FvExpr -> LL FvExpr
llExpr old_expr = do
  new_expr <-
    case old_expr of
      Lam { _annot, _patns, _body } -> do
        global_ident <- fresh Lambda
        llLamAs global_ident _annot _patns _body
      Let { _isrec, _defns = old_defns, _body = old_body }
        | _isrec && Set.null (fvRecDefns old_defns) -> do
          let (local_idents, _, old_rhss) = unzipDefns3 old_defns
          renaming <-
            forM local_idents $ \ident -> (,) ident <$> within ident (fresh Nice)
          forM_ (zip renaming old_rhss) $ \((local_ident, global_ident), old_rhs) ->
            within local_ident $ llExprAs global_ident (rename renaming old_rhs)
          let renamed_body = rename renaming old_body
          llExpr renamed_body
        | otherwise -> do
          let (lift_defns, keep_defns) =
                partition (\MkDefn{ _expr } -> Set.null (annot _expr)) old_defns
          renaming <- forM lift_defns $
            \MkDefn{ _patn = MkPatn{ _ident = local_ident }, _expr } ->
              within local_ident $ do
                global_ident <- fresh Nice
                llExprAs global_ident _expr
                return (local_ident, global_ident)
          let renameAndLiftLazy expr = llExpr (rename renaming expr)
          new_defns <- forM keep_defns $
            \defn@MkDefn{ _patn = MkPatn{ _ident }, _expr = old_rhs } ->
              within _ident $ do
                new_rhs <-
                  if _isrec then renameAndLiftLazy old_rhs else llExpr old_rhs
                return (defn { _expr = new_rhs } :: FvDefn)
          new_body <- renameAndLiftLazy old_body
          return $ old_expr { _defns = new_defns, _body = new_body }
      -- The remaining cases are boilerplate.
      Var { }  -> return old_expr
      Num { }  -> return old_expr
      Pack { } -> return old_expr
      Ap { _fun, _arg } -> do
        _fun <- llExpr _fun
        _arg <- llExpr _arg
        return $ old_expr { _fun, _arg }
      ApOp { _arg1, _arg2 } -> do
        _arg1 <- llExpr _arg1
        _arg2 <- llExpr _arg2
        return $ old_expr { _arg1, _arg2 }
      If { _cond, _then, _else } -> do
        _cond <- llExpr _cond
        _then <- llExpr _then
        _else <- llExpr _else
        return $ old_expr { _cond, _then, _else }
      Rec {} -> undefined
      Sel {} -> undefined
  return $ fvAdjust new_expr

fvRecDefns :: [FvDefn] -> Set Ident
fvRecDefns defns =
  let (idents, _, rhss) = unzipDefns3 defns
      fv_rhss = Set.unions (map annot rhss)
  in  fv_rhss `Set.difference` Set.fromList idents

rename :: [(Ident, Ident)] -> FvExpr -> FvExpr
rename table expr =
  runReader (runRewrite (emptyRewrite { rewrite_expr }) expr) (Map.fromList table)
  where
    -- TODO: Stop when everything to rename gets bound.
    localize idents = local (\env -> foldr Map.delete env idents)
    rewrite_expr descend old_expr = do
      post_expr <-
        case old_expr of
          Var { _ident = old_ident } -> do
            new_ident_opt <- asks (Map.lookup old_ident)
            case new_ident_opt of
              Nothing        -> return $ old_expr
              Just new_ident -> return $ mkVar Global new_ident
          Lam { _patns, _body } -> do
            let (bound_idents, _) = unzipPatns _patns
            localize bound_idents (descend old_expr)
          Let { _isrec = True, _defns, _body } -> do
            let (bound_idents, _, _) = unzipDefns3 _defns
            localize bound_idents (descend old_expr)
          Let { _isrec = False, _defns, _body = old_body } -> do
            -- This is kind of hacky. Be careful when refactoring.
            post_expr' <- descend old_expr
            let (bound_idents, _, _) = unzipDefns3 _defns
            post_body <- localize bound_idents (rewrite_expr descend old_body)
            return $ post_expr' { _body = post_body }
          -- The remaining cases are boilerplate.
          Num  {} -> descend old_expr
          Pack {} -> descend old_expr
          Ap   {} -> descend old_expr
          ApOp {} -> descend old_expr
          If   {} -> descend old_expr
          Rec  {} -> descend old_expr
          Sel  {} -> descend old_expr
      return $ fvAdjust post_expr
