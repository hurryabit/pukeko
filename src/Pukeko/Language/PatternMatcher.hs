module Pukeko.Language.PatternMatcher
  ( compileModule
  )
where

import Control.Monad
import Text.Parsec (SourcePos)

import Pukeko.Error
import Pukeko.Language.Syntax
import Pukeko.Language.Type (ADT (..), Constructor (..))
import qualified Pukeko.Language.Ident as Ident
import qualified Pukeko.Language.Rewrite as Rewrite

type PM a = Except String a

altnWith :: Ident.Con -> Altn StageTR a -> Bool
altnWith name MkAltn{_con = MkConstructor{_name}} = name == _name

pmExpr :: Expr StageTR SourcePos -> PM (Expr StageTR SourcePos)
pmExpr expr = case expr of
  Match{_annot, _expr, _altns} -> case _altns of
    [] -> bug "pattern matcher" "zero alternatives" Nothing
    MkAltn{_con = MkConstructor{_adt = MkADT{_constructors}}}:_ -> do
      _altns <- forM _constructors $ \MkConstructor{_name} -> do
        case filter (altnWith _name) _altns of
          []    -> throwAt _annot "unmatched term cons" _name
          _:_:_ -> throwAt _annot "multiply matched term cons" _name
          [altn@MkAltn{_rhs}] -> do
            _rhs <- pmExpr _rhs
            return (altn{_rhs} :: Altn _ _)
      _expr <- pmExpr _expr
      return expr{_expr, _altns}
  _ -> Rewrite.expr pmExpr expr

compileModule :: MonadError String m => Module StageTR SourcePos -> m (Module StageTR SourcePos)
compileModule = runExcept . Rewrite.module_ pmExpr
