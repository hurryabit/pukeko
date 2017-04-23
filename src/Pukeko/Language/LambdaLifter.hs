{-# LANGUAGE TemplateHaskell #-}
module Pukeko.Language.LambdaLifter
  ( liftModule
  )
  where

import Control.Monad.RWS hiding (asks, local, gets)
import Data.Label (mkLabels)
import Data.Label.Monadic
import Data.Set (Set)

import qualified Data.Set as Set

import Pukeko.Language.Syntax

type FvExpr = Expr (Set Ident)
type FvDefn = Defn (Set Ident)
type FvBind = Bind (Set Ident)
type FvAltn = Altn (Set Ident)

data Env = MkEnv{ _bound :: Set Ident }
mkLabels [''Env]

newtype LL a = MkLL { unLL :: RWS Env [FvDefn] [Ident] a }
  deriving ( Functor, Applicative, Monad
           , MonadReader Env
           , MonadWriter [FvDefn]
           , MonadState [Ident]
           )

freshIdent :: LL Ident
freshIdent = state $ \(ident:idents) -> (ident, idents)

liftModule :: Module a -> FvExpr
liftModule module_ =
  let _defns = do
        Def{ _defns } <- module_
        defn <- _defns
        liftTopDefn defn
  in Let{ _annot = Set.empty
        , _isrec = True
        , _defns
        , _body  = mkVar True (MkIdent "main")
        }

-- TODO: Fix the awful hack for the right naming of non-CAFs.
liftTopDefn :: Defn a -> [FvDefn]
liftTopDefn defn@MkDefn{ _bind = MkBind{ _ident }, _expr } =
  let env = MkEnv{ _bound = Set.empty }
      st0 = map (\n -> MkIdent $ unIdent _ident ++ '$':show n) [1 :: Int ..]
      is_lambda = case _expr of
        Lam{} -> True
        _     -> False
      st | is_lambda = _ident : st0
         | otherwise = st0
      (defn1, defns) = evalRWS (unLL $ liftDefn defn) env st
  in  if is_lambda then defns else defns ++ [defn1]

liftBind :: Bind a -> LL FvBind
liftBind MkBind{ _ident, _type } = do
  let _annot = Set.singleton _ident
  return MkBind{ _annot, _ident, _type }

liftDefn :: Defn a -> LL FvDefn
liftDefn MkDefn{ _bind, _expr } = do
  _bind <- liftBind _bind
  _expr <- liftExpr _expr
  return MkDefn{ _bind, _expr }

liftAltn :: Altn a -> LL FvAltn
liftAltn MkAltn{ _cons, _binds, _rhs } = do
  _binds <- mapM liftBind _binds
  _rhs  <- localize _binds (liftExpr _rhs)
  let _annot = annot _rhs `Set.difference` Set.unions (map annot _binds)
  return MkAltn{ _annot, _cons, _binds, _rhs }

liftExpr :: Expr a -> LL FvExpr
liftExpr expr = case expr of
  Var{ _ident } -> do
    _bound <- asks bound
    let _annot | _ident `Set.member` _bound = Set.singleton _ident
               | otherwise                  = Set.empty
    return Var{ _annot, _ident }
  Num{ _int } -> return Num{ _annot = Set.empty, _int }
  Ap{ _fun, _args } -> do
    _fun  <- liftExpr _fun
    _args <- mapM liftExpr _args
    let _annot = annot _fun `Set.union` Set.unions (map annot _args)
    return Ap{ _annot, _fun, _args }
  ApOp{ _op, _arg1, _arg2 } -> do
    _arg1 <- liftExpr _arg1
    _arg2 <- liftExpr _arg2
    let _annot = annot _arg1 `Set.union` annot _arg2
    return ApOp{ _annot, _op, _arg1, _arg2 }
  Let{ _isrec, _defns, _body } -> do
    let binds = map _bind _defns
    _defns <- localize (if _isrec then binds else []) (mapM liftDefn _defns)
    _body  <- localize binds (liftExpr _body)
    let (binds, rhss) = unzipDefns _defns
        fv_binds = Set.unions (map annot binds)
        fv_rhss  = Set.unions (map annot rhss)
        fv_body  = annot _body
        _annot
          | _isrec    = (fv_body `Set.union` fv_rhss) `Set.difference` fv_binds
          | otherwise = (fv_body `Set.difference` fv_binds) `Set.union` fv_rhss
    return Let{ _annot, _isrec, _defns, _body }
  Lam{ _binds, _body } -> do
    _ident <- freshIdent
    _body <- localize _binds (liftExpr _body)
    _binds <- mapM liftBind _binds
    let _annot = annot _body `Set.difference` Set.unions (map annot _binds)
        frees = Set.toList _annot
    let rhs = Lam{ _annot = Set.empty, _binds = map mkBind frees ++ _binds, _body }
    tell [MkDefn{ _bind = mkBind _ident, _expr = rhs }]
    return Ap{ _annot, _fun = mkVar False _ident, _args = map (mkVar True) frees }
  If{ _cond, _then, _else } -> do
    _cond <- liftExpr _cond
    _then <- liftExpr _then
    _else <- liftExpr _else
    let _annot = Set.unions $ map annot [_cond, _then, _else]
    return If{ _annot, _cond, _then, _else }
  Match{ _expr, _altns } -> do
    _expr  <- liftExpr _expr
    _altns <- mapM liftAltn _altns
    let _annot = annot _expr `Set.union` Set.unions (map annot _altns)
    return Match{ _annot, _expr, _altns }

mkBind :: Ident -> FvBind
mkBind _ident = MkBind{ _annot = Set.singleton _ident, _ident, _type = Nothing }

mkVar :: Bool -> Ident -> FvExpr
mkVar is_local _ident =
  let _annot = if is_local then Set.singleton _ident else Set.empty
  in  Var{ _annot, _ident }

localize :: [Bind a] -> LL b -> LL b
localize binds ll = do
  let (idents, _) = unzipBinds binds
  local bound (Set.fromList idents `Set.union`) ll
