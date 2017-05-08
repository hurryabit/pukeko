module Pukeko.Language.FreeVars
  ( FreeVars
  , annotModule
  )
where

import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set

import Pukeko.Language.Syntax
import qualified Pukeko.Language.Ident as Ident

type FreeVars = Set Ident.EVar

newtype FV a = FV { unFV :: Reader FreeVars a }
  deriving ( Functor, Applicative, Monad
           , MonadReader FreeVars
           )

runFV :: FV a -> a
runFV fv = runReader (unFV fv) Set.empty

localize :: FreeVars -> FV a -> FV a
localize fvs = local (fvs `Set.union`)

fvBind :: Bind stage a -> FV (Bind stage FreeVars)
fvBind bind@MkBind{ _ident } =
  return (bind{ _annot = Set.singleton _ident } :: Bind _ _)

fvBind0 :: Bind0 stage a -> FV (Bind0 stage FreeVars)
fvBind0 bind0@MkBind{ _ident } =
  return (bind0{ _annot = maybe Set.empty Set.singleton _ident } :: Bind0 _ _)

fvDefn :: Defn stage a -> FV (Defn stage FreeVars)
fvDefn MkDefn{ _lhs, _rhs } = MkDefn <$> fvBind _lhs <*> fvExpr _rhs

fvAltn :: Altn stage a -> FV (Altn stage FreeVars)
fvAltn altn@MkAltn{ _binds, _rhs } = do
  _binds <- mapM fvBind0 _binds
  let fv_binds = Set.unions (map annot _binds)
  _rhs <- localize fv_binds $ fvExpr _rhs
  let _annot = annot _rhs `Set.difference` fv_binds
  return altn{ _annot, _binds, _rhs }

fvExpr :: Expr stage a -> FV (Expr stage FreeVars)
fvExpr expr = case expr of
  Var{ _var } -> do
    is_local <- asks (_var `Set.member`)
    let _annot
          | is_local  = Set.singleton _var
          | otherwise = Set.empty
    return Var{ _annot, _var }
  Con{ _con } -> return Con{ _annot = Set.empty, _con }
  Num{ _int } -> return Num{ _annot = Set.empty, _int }
  Ap{ _fun, _args } -> do
    _fun <- fvExpr _fun
    _args <- mapM fvExpr _args
    let _annot = annot _fun `Set.union` Set.unions (map annot _args)
    return Ap{ _annot, _fun, _args }
  Lam{ _binds, _body } -> do
    _binds <- mapM fvBind0 _binds
    let fv_binds = Set.unions (map annot _binds)
    _body <- localize fv_binds $ fvExpr _body
    let _annot = annot _body `Set.difference` fv_binds
    return Lam{ _annot, _binds, _body }
  Let{ _isrec, _defns, _body } -> do
    let (lhss, rhss) = unzipDefns _defns
    lhss <- mapM fvBind lhss
    let fv_lhss = Set.unions (map annot lhss)
    rhss <- localize (if _isrec then fv_lhss else Set.empty) $ mapM fvExpr rhss
    _body <- localize fv_lhss $ fvExpr _body
    let fv_rhss = Set.unions (map annot rhss)
        fv_body = annot _body
        _annot | _isrec    = (fv_body `Set.union` fv_rhss) `Set.difference` fv_lhss
               | otherwise = (fv_body `Set.difference` fv_lhss) `Set.union` fv_rhss
        _defns = zipWith MkDefn lhss rhss
    return Let{ _annot, _isrec, _defns, _body }
  If{ _cond, _then, _else } -> do
    _cond <- fvExpr _cond
    _then <- fvExpr _then
    _else <- fvExpr _else
    let _annot = Set.unions (map annot [_cond, _then, _else])
    return If{ _annot, _cond, _then, _else }
  Match{ _expr, _altns } -> do
    _expr <- fvExpr _expr
    _altns <- mapM fvAltn _altns
    let _annot = annot _expr `Set.union` Set.unions (map annot _altns)
    return Match{ _annot, _expr, _altns }

fvTopLevel :: TopLevel stage a -> FV (TopLevel stage FreeVars)
fvTopLevel top = case top of
  Type{ _adts} -> return Type{ _annot = Set.empty, _adts }
  Val{ _ident, _type } -> return Val{ _annot = Set.empty, _ident, _type }
  Def{ _isrec, _defns } -> do
    _defns <- mapM fvDefn _defns
    return Def{ _annot = Set.empty, _isrec, _defns }
  Asm{ _ident, _asm } -> return Asm{ _annot = Set.empty, _ident, _asm }

fvModule :: Module stage a -> FV (Module stage FreeVars)
fvModule = mapM fvTopLevel

annotModule :: Module stage a -> Module stage FreeVars
annotModule = runFV . fvModule
