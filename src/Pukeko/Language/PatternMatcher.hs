module Pukeko.Language.PatternMatcher
  ( compileModule
  )
where

import Control.Monad.Except
import Text.Parsec (SourcePos)

import Pukeko.Language.Syntax
import Pukeko.Language.Type (ADT (..), Constructor (..))
import qualified Pukeko.Language.Ident as Ident
import qualified Pukeko.Language.Rewrite as Rewrite

type PM a = Except String a

-- TODO: Put this in some utils module.
throwHere :: MonadError String m => SourcePos -> String -> m a
throwHere annot msg = throwError $ show annot ++ ": " ++ msg

altnWith :: Ident.Con -> Altn StageTR a -> Bool
altnWith name MkAltn{_con = MkConstructor{_name}} = name == _name

pmExpr :: Expr StageTR SourcePos -> PM (Expr StageTR SourcePos)
pmExpr expr = case expr of
  Match{_annot, _expr, _altns} -> case _altns of
    [] -> error "BUG: zero alternatives in pattern matcher"
    MkAltn{_con = MkConstructor{_adt = MkADT{_constructors}}}:_ -> do
      _altns <- forM _constructors $ \MkConstructor{_name} -> do
        case filter (altnWith _name) _altns of
          []    -> throwHere _annot $ "match does not mention " ++ show _name
          _:_:_ -> throwHere _annot $ "match mentions " ++ show _name ++ " multiple times"
          [altn@MkAltn{_rhs}] -> do
            _rhs <- pmExpr _rhs
            return (altn{_rhs} :: Altn _ _)
      _expr <- pmExpr _expr
      return expr{_expr, _altns}
  _ -> Rewrite.expr pmExpr expr

compileModule :: MonadError String m => Module StageTR SourcePos -> m (Module StageTR SourcePos)
compileModule = either throwError return . runExcept . Rewrite.module_ pmExpr
