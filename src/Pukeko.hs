module Pukeko
  ( AST.Module (..)
  , compileToCore
  )
  where

import Pukeko.Error

import qualified Pukeko.Language.AST.Std        as AST
import qualified Pukeko.Language.CoreCompiler   as CoreCompiler
import qualified Pukeko.Language.DeadCode       as DeadCode
import qualified Pukeko.Language.Renamer        as Renamer
import qualified Pukeko.Language.KindChecker    as KindChecker
import qualified Pukeko.Language.LambdaLifter   as LambdaLifter
import qualified Pukeko.Language.Parser         as Parser
import qualified Pukeko.Language.PatternMatcher as PatternMatcher
import qualified Pukeko.Language.TypeChecker    as TypeChecker
import qualified Pukeko.Language.TypeResolver   as TypeResolver
import qualified Pukeko.Language.FunResolver    as FunResolver

compileToCore
  :: MonadError String m
  => Parser.Module
  -> m (CoreCompiler.Module, LambdaLifter.Module)
compileToCore module_ = do
  module_ll <- return (Renamer.renameModule module_)
               >>= TypeResolver.resolveModule
               >>= KindChecker.checkModule
               >>= FunResolver.resolveModule
               >>= TypeChecker.checkModule
               >>= PatternMatcher.compileModule
               >>= return . DeadCode.cleanModule
               >>= return . LambdaLifter.liftModule
  let module_cc = CoreCompiler.compileModule module_ll
  return (module_cc, module_ll)
