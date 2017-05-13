{-# LANGUAGE TemplateHaskell #-}
module Pukeko.Language.TypeResolver
  ( resolve
  )
where

import Control.Monad
import Control.Monad.State hiding (gets, modify)
import Data.Label (mkLabels)
import Data.Label.Monadic
import Data.Map (Map)
import Text.Parsec (SourcePos)
import qualified Data.Map as Map

import Pukeko.Error
import Pukeko.Language.Syntax
import Pukeko.Language.Type
import qualified Pukeko.Language.Ident as Ident

data TRState = MkTRState
  { _types   :: Map Ident.Con (ADT Ident.Con)
  , _constrs :: Map Ident.Con (Constructor (ADT Ident.Con))
  }
mkLabels [''TRState]

newtype TR a = TR {unTR :: ExceptT String (State TRState) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadState TRState
           )

runTR :: MonadError String m => TR a -> m a
runTR tr =
  let st = MkTRState{_types = Map.empty, _constrs= Map.empty}
  in  evalState (runExceptT (unTR tr)) st

trType :: SourcePos
       -> Type (TypeConOf StageLP) Closed -> TR (Type (TypeConOf StageTR) Closed)
trType posn typ = case typ of
    QVar var -> return $ QVar var
    TFun tx ty -> TFun <$> trType posn tx <*> trType posn ty
    TApp con typs -> do
      adt_opt <- Map.lookup con <$> gets types
      case adt_opt of
        Nothing -> throwAt posn "unknown type cons" con
        Just adt -> TApp adt <$> traverse (trType posn) typs

-- TODO: Have only one insert function.
insertTypeCon :: SourcePos -> ADT Ident.Con -> TR ()
insertTypeCon posn adt@MkADT{_name} = do
  conflict <- Map.member _name <$> gets types
  when conflict $ throwAt posn "duplicate type cons" _name
  modify types (Map.insert _name adt)

insertTermCon :: SourcePos -> Constructor (ADT Ident.Con) -> TR ()
insertTermCon posn con@MkConstructor{_name} = do
  conflict <- Map.member _name <$> gets constrs
  when conflict $
    throwAt posn "duplicate term cons" _name
  modify constrs (Map.insert _name con)

findTermCon :: SourcePos -> Ident.Con -> TR (TermConOf StageTR)
findTermCon posn name = do
  con_opt <- Map.lookup name <$> gets constrs
  case con_opt of
    Nothing -> throwAt posn "unknown term cons" name
    Just con -> return con

trBind :: BindGen i StageLP SourcePos -> TR (BindGen i StageTR SourcePos)
trBind MkBind{_annot, _ident} = do
  return MkBind{_annot, _ident}

trDefn :: Defn StageLP SourcePos -> TR (Defn StageTR SourcePos)
trDefn defn@MkDefn{_lhs, _rhs} = do
  _lhs <- trBind _lhs
  _rhs <- trExpr _rhs
  return defn{_lhs, _rhs}

trAltn :: Altn StageLP SourcePos -> TR (Altn StageTR SourcePos)
trAltn altn@MkAltn{_annot, _con, _binds, _rhs} = do
  _con <- findTermCon _annot _con
  _binds <- traverse trBind _binds
  _rhs <- trExpr _rhs
  return altn{_con, _binds, _rhs}

trExpr :: Expr StageLP SourcePos -> TR (Expr StageTR SourcePos)
trExpr expr = case expr of
    Var{_var} -> return expr{_var}
    Con{_annot, _con} -> do
      _con <- findTermCon _annot _con
      return (expr{_con} :: Expr _ _)
    Num{_int} -> return expr{_int}
    Ap{_fun, _args} -> do
      _fun <- trExpr _fun
      _args <- traverse trExpr _args
      return expr{_fun, _args}
    Lam{_binds, _body} -> do
      _binds <- traverse trBind _binds
      _body <- trExpr _body
      return (expr{_binds, _body} :: Expr _ _)
    Let{_defns, _body} -> do
      _defns <- traverse trDefn _defns
      _body <- trExpr _body
      return expr{_defns, _body}
    If{_cond, _then, _else} -> do
      _cond <- trExpr _cond
      _then <- trExpr _then
      _else <- trExpr _else
      return expr{_cond, _then, _else}
    Match{_altns, _expr} -> do
      _expr <- trExpr _expr
      _altns <- traverse trAltn _altns
      return expr{_altns, _expr}

trTopLevel :: TopLevel StageLP SourcePos -> TR (TopLevel StageTR SourcePos)
trTopLevel top = case top of
  Type{_annot, _adts} -> do
    forM_ _adts (insertTypeCon _annot)
    _adts <- forM _adts $ \_adt@MkADT{_constructors} -> do
      _constructors <- forM _constructors $ \con@MkConstructor{_fields} -> do
        _fields <- traverse (trType _annot) _fields
        let con' = con{_adt, _fields}
        insertTermCon _annot con'
        return con'
      return _adt{_constructors}
    return top{_adts}
  Val{_annot, _type} -> do
    _type <- trType _annot _type
    return (top{_type} :: TopLevel _ _)
  Def{_defns} -> do
    _defns <- traverse trDefn _defns
    return (top{_defns} :: TopLevel _ _)
  Asm{_asm} -> return top{_asm}

trModule :: Module StageLP SourcePos -> TR (Module StageTR SourcePos)
trModule = traverse trTopLevel

resolve :: MonadError String m => Module StageLP SourcePos -> m (Module StageTR SourcePos)
resolve = runTR . trModule
