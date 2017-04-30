module Pukeko.Language.PatternMatcher
  ( compileModule
  )
where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Map (Map)
import Text.Parsec (SourcePos)
import qualified Data.Map as Map

import Pukeko.Language.ADT
import Pukeko.Language.Syntax
import qualified Pukeko.Language.Ident as Ident
import qualified Pukeko.Language.Rewrite as Rewrite

type Env = Map Ident.Con Constructor

newtype PM a = PM{ unPM :: ExceptT String (Reader Env) a}
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader Env
           )

-- TODO: Put this in some utils module.
throwHere :: MonadError String m => SourcePos -> String -> m a
throwHere annot msg = throwError $ show annot ++ ": " ++ msg

resolve :: Ident.Con -> PM Constructor
resolve ident = do
  constr_opt <- asks (ident `Map.lookup`)
  case constr_opt of
    Nothing -> error "BUG: unknown constructor in pattern matcher"
    Just constr -> return constr

pmExpr :: Expr SourcePos -> PM (Expr SourcePos)
pmExpr expr = case expr of
  Match{_annot, _expr, _altns} -> case _altns of
    [] -> error "BUG: zero alternatives in pattern matcher"
    MkAltn{_con}:_ -> do
      MkConstructor{_adt = MkADT{_constructors}} <- resolve _con
      _altns <- forM _constructors $ \MkConstructor{_name} -> do
        case filter (\MkAltn{_con} -> _con == _name) _altns of
          []    -> throwHere _annot $ "match does not mention " ++ show _name
          _:_:_ -> throwHere _annot $ "match mentions " ++ show _name ++ " multiple times"
          [altn@MkAltn{_rhs}] -> do
            _rhs <- pmExpr _rhs
            return (altn{_rhs} :: Altn _)
      _expr <- pmExpr _expr
      return expr{_expr, _altns}
  _ -> Rewrite.expr pmExpr expr

compileModule :: MonadError String m =>
                 Map Ident.Con Constructor -> Module SourcePos -> m (Module SourcePos)
compileModule env module_ =
  either throwError return $
  runReader (runExceptT (unPM (Rewrite.module_ pmExpr module_))) env
