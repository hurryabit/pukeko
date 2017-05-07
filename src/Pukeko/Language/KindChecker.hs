module Pukeko.Language.KindChecker
  ( check
  )
where

import Control.Monad.Except
import Data.Maybe (catMaybes)
import Text.Parsec (SourcePos)
import qualified Data.Set as Set

import Pukeko.Language.Syntax
import Pukeko.Language.Type hiding (Type)
import qualified Pukeko.Language.Ident   as Ident
import qualified Pukeko.Language.Rewrite as Rewrite
import qualified Pukeko.Language.Type    as Type

type Type a = Type.Type (ADT Ident.Con) a

type KC a = Except String a

runKC :: MonadError String m => KC a -> m a
runKC kc = case runExcept kc of
  Left error -> throwError error
  Right res -> return res

throwHere :: SourcePos -> String -> KC a
throwHere annot msg = throwError $ show annot ++ ": " ++ msg

kcType :: SourcePos -> Type a -> KC (Type a)
kcType posn typ = case typ of
    TApp MkADT{_name, _params} typs
      -- TODO: Check that params are mutually distinct.
      | length _params /= length typs ->
          throwHere posn $
          "Type constructor " ++ show _name ++ " expects "
          ++ show (length _params) ++ " parameters"
    _ -> Rewrite.type_ (kcType posn) typ

kcBind :: BindGen i StageTR SourcePos -> KC (BindGen i StageTR SourcePos)
kcBind bind@MkBind{_annot, _type} = do
  _type <- traverse (kcType _annot) _type
  return (bind{_type} :: BindGen _ StageTR _)

-- TODO: Look for a way to make this a one liner.
kcDefnLhs :: Defn StageTR SourcePos -> KC (Defn StageTR SourcePos)
kcDefnLhs defn@MkDefn{_lhs} = do
  _lhs <- kcBind _lhs
  return defn{_lhs}

kcAltnLhs :: Altn StageTR SourcePos -> KC (Altn StageTR SourcePos)
kcAltnLhs altn@MkAltn{_binds} = do
  _binds <- traverse kcBind _binds
  return (altn{_binds} :: Altn _ _)

kcExpr :: Expr StageTR SourcePos -> KC (Expr StageTR SourcePos)
kcExpr expr = do
  expr <- case expr of
    Lam{_binds} -> do
      _binds <- traverse kcBind _binds
      return (expr{_binds} :: Expr _ _)
    Let{_defns} -> do
      _defns <- traverse kcDefnLhs _defns
      return (expr{_defns} :: Expr _ _)
    Match{_altns} -> do
      _altns <- traverse kcAltnLhs _altns
      return expr{_altns}
    _ -> return expr
  Rewrite.expr kcExpr expr

kcTopLevel :: TopLevel StageTR SourcePos -> KC (Maybe (TopLevel StageTR SourcePos))
kcTopLevel top = case top of
  Type{_annot, _adts} -> do
    forM_ _adts $ \MkADT{_params, _constructors} -> do
      forM_ _constructors $ \MkConstructor{_name, _fields} -> do
        let unbound =
              Set.unions (map qvars _fields) `Set.difference` Set.fromList _params
        unless (Set.null unbound) $
          throwHere _annot $
          "Unbound type variables in constructor " ++ show _name ++ ": "
          ++ unwords (map show $ Set.toList unbound)
        mapM_ (kcType _annot) _fields
    return Nothing
  Val{_annot, _type} -> do
    _type <- kcType _annot _type
    return $ Just (top{_type} :: TopLevel StageTR _)
  Def{_defns} -> do
    _defns <- traverse (Rewrite.defn kcExpr) _defns
    return $ Just (top{_defns} :: TopLevel _ _)
  Asm{} -> return $ Just top

kcModule :: Module StageTR SourcePos -> KC (Module StageTR SourcePos)
kcModule module_ = catMaybes <$> traverse kcTopLevel module_

check :: MonadError String m => Module StageTR SourcePos -> m (Module StageTR SourcePos)
check = runKC . kcModule
