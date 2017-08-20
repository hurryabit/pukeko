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

fvPatn :: Patn stage a -> FV (Patn stage FreeVars)
fvPatn patn = return $ case patn of
  Wild{}       -> Wild{_annot = Set.empty}
  Bind{_ident} -> Bind{_annot = Set.singleton _ident, _ident}

fvDefn :: Defn stage a -> FV (Defn stage FreeVars)
fvDefn MkDefn{_lhs, _rhs} = do
  _rhs <- fvExpr _rhs
  let _annot = annot _rhs
  return MkDefn{_annot, _lhs, _rhs}

fvAltn :: Altn stage a -> FV (Altn stage FreeVars)
fvAltn altn@MkAltn{_patns, _rhs} = do
  _patns <- mapM fvPatn _patns
  let fv_patns = Set.unions (map annot _patns)
  _rhs <- localize fv_patns $ fvExpr _rhs
  let _annot = annot _rhs `Set.difference` fv_patns
  return altn{_annot, _patns, _rhs}

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
  Lam{_patns, _body} -> do
    _patns <- traverse fvPatn _patns
    let fv_patns = Set.unions (map annot _patns)
    _body <- localize fv_patns $ fvExpr _body
    let _annot = annot _body `Set.difference` fv_patns
    return Lam{_annot, _patns, _body}
  Let{ _isrec, _defns, _body } -> do
    let (lhss, _) = unzipDefns _defns
    let fv_lhss = Set.fromList lhss
    _defns <- localize (if _isrec then fv_lhss else Set.empty) $ traverse fvDefn _defns
    _body <- localize fv_lhss $ fvExpr _body
    let fv_defns = Set.unions (map annot _defns)
        fv_body = annot _body
        _annot | _isrec    = (fv_body `Set.union` fv_defns) `Set.difference` fv_lhss
               | otherwise = (fv_body `Set.difference` fv_lhss) `Set.union` fv_defns
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
