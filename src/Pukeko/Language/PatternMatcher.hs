module Pukeko.Language.PatternMatcher
  ( Module
  , compileModule
  )
where

import           Data.Traversable   (for)

import           Pukeko.Error
import           Pukeko.Language.Base.AST
import           Pukeko.Language.PatternMatcher.AST
import qualified Pukeko.Language.TypeChecker.AST    as In
import qualified Pukeko.Language.Type               as Ty
import qualified Pukeko.Language.Ident              as Id

type PM a = Except String a

name :: Ty.Constructor _ -> Id.Con
name Ty.MkConstructor{_name} = _name

pmExpr :: In.Expr v -> PM (Expr v)
pmExpr = \case
  In.Var w x       -> pure $ Var w x
  In.Con w c       -> pure $ Con w c
  In.Num w n       -> pure $ Num w n
  In.App w t  us   -> App w <$> pmExpr t <*> traverse pmExpr us
  -- In.If  w t  u  v -> If  w <$> pmExpr t <*> pmExpr u <*> pmExpr v
  In.Lam w bs t    -> Lam w bs <$> pmExpr t
  In.Let w ds t    -> Let w <$> (traverse . rhs2) pmExpr ds <*> pmExpr t
  In.Rec w ds t    -> Rec w <$> (traverse . rhs2) pmExpr ds <*> pmExpr t
  In.Mat w t  as0  ->
    case as0 of
      []   -> throwDocAt w "match without alternatives"
      (MkAltn _ con0 _ _):_ -> do
        let Ty.MkConstructor{_adt = Ty.MkADT{_constructors}} = con0
        as1 <- for _constructors $ \con1 -> do
          case filter (\(MkAltn _ con2 _ _) -> name con1 == name con2) as0 of
            [] -> throwAt w "unmatched constructor" (name con1)
            _:_:_ -> throwAt w "multiply matched constructor" (name con1)
            [a] -> return a
        Mat w <$> pmExpr t <*> traverse (altnRhs pmExpr) as1

compileModule :: MonadError String m => In.Module -> m Module
compileModule = runExcept . (traverse . topLevelExpr) pmExpr
