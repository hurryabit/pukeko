module Pukeko
  ( Parser.Module
  , parse
  , compileToCore
  -- , compileToCore
  -- , parse
  )
  where

import Pukeko.Error

import qualified Pukeko.Language.CoreCompiler   as CoreCompiler
import qualified Pukeko.Language.DeadCode       as DeadCode
import qualified Pukeko.Language.DeBruijner     as DeBruijner
import qualified Pukeko.Language.KindChecker    as KindChecker
import qualified Pukeko.Language.LambdaLifter   as LambdaLifter
import qualified Pukeko.Language.Parser         as Parser
import qualified Pukeko.Language.PatternMatcher as PatternMatcher
import qualified Pukeko.Language.TypeChecker    as TypeChecker
import qualified Pukeko.Language.TypeResolver   as TypeResolver

parse :: MonadError String m => String -> String -> m Parser.Module
parse = Parser.parseModule

compileToCore
  :: MonadError String m
  => Parser.Module
  -> m (CoreCompiler.Module, LambdaLifter.Module)
compileToCore module_ = do
  module_ll <- return (DeBruijner.indexModule module_)
               >>= TypeResolver.resolveModule
               >>= KindChecker.checkModule
               >>= TypeChecker.checkModule
               >>= PatternMatcher.compileModule
               >>= return . DeadCode.cleanModule
               >>= return . LambdaLifter.liftModule
  let module_cc = CoreCompiler.compileModule module_ll
  return (module_cc, module_ll)
