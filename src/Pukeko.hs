module Pukeko
  ( Module (..)
  , compileToCore
  )
  where

import Pukeko.Error

import           Pukeko.Language.AST.Std        (Module (..))
import qualified Pukeko.Language.CoreCompiler   as CoreCompiler
import qualified Pukeko.Language.DeadCode       as DeadCode
import qualified Pukeko.Language.Renamer        as Renamer
import qualified Pukeko.Language.KindChecker    as KindChecker
import qualified Pukeko.Language.LambdaLifter   as LambdaLifter
import qualified Pukeko.Language.Parser         as Parser
import qualified Pukeko.Language.PatternMatcher as PatternMatcher
import qualified Pukeko.Language.Inferencer     as Inferencer
import qualified Pukeko.Language.TypeChecker    as TypeChecker
import qualified Pukeko.Language.TypeEraser     as TypeEraser
import qualified Pukeko.Language.TypeResolver   as TypeResolver
import qualified Pukeko.Language.FunResolver    as FunResolver

compileToCore
  :: MonadError String m
  => Bool
  -> Parser.Module
  -> m (CoreCompiler.Module, Module LambdaLifter.Out, Module Inferencer.Out)
compileToCore _unsafe module_pu = do
  module_ti <- pure module_pu
               >>= Renamer.renameModule
               >>= TypeResolver.resolveModule
               >>= FunResolver.resolveModule
               >>= KindChecker.checkModule
               >>= Inferencer.inferModule
  module_ll <- pure module_ti
               >>= PatternMatcher.compileModule
               >>= TypeChecker.checkModule
               >>= pure . TypeEraser.eraseModule
               >>= pure . LambdaLifter.liftModule
               >>= pure . DeadCode.cleanModule
  let module_cc = CoreCompiler.compileModule module_ll
  return (module_cc, module_ll, module_ti)
