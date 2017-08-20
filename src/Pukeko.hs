module Pukeko
  ( ModuleLP
  , compileToCore
  , parse
  )
  where

import Control.Monad.Except
import Text.Parsec (SourcePos)

import Pukeko.Language.Syntax
import qualified Pukeko.Core.Syntax             as Core
import qualified Pukeko.Language.CoreCompiler   as CoreCompiler
import qualified Pukeko.Language.DeadCode       as DeadCode
import qualified Pukeko.Language.FreeVars       as FreeVars
import qualified Pukeko.Language.KindChecker    as KindChecker
import qualified Pukeko.Language.LambdaLifter   as Lifter
import qualified Pukeko.Language.Parser         as Parser
import qualified Pukeko.Language.PatternMatcher as PatternMatcher
import qualified Pukeko.Language.TypeChecker    as TypeChecker
import qualified Pukeko.Language.TypeResolver   as TypeResolver

type ModuleLP = Module StageLP SourcePos

parse :: MonadError String m => String -> String -> m ModuleLP
parse = Parser.parseModule

compileToCore :: MonadError String m
              => Module StageLP SourcePos
              -> m (Core.Module, Module StageTR _)
compileToCore module_ = do
  module_ <- TypeResolver.resolve module_
  module_ <- KindChecker.check module_
  TypeChecker.checkModule module_
  module_ <- PatternMatcher.compileModule module_
  module_ <- pure $ FreeVars.annotModule module_
  module_ <- pure $ DeadCode.eliminate module_
  module_ll <- pure $ Lifter.liftModule module_
  module_cc <- pure $ CoreCompiler.compileModule module_ll
  return (module_cc, module_ll)
