{-# LANGUAGE RankNTypes #-}
module Pukeko
  ( Module (..)
  , compileToCore
  )
  where

import           Control.Monad ((>=>))
import           Pukeko.Error

import           Pukeko.Language.AST.Std        (Module (..))
import           Pukeko.Language.AST.Stage      (Typed)
import qualified Pukeko.Language.CoreCompiler   as CoreCompiler
import qualified Pukeko.Language.DeadCode       as DeadCode
import qualified Pukeko.Language.Renamer        as Renamer
import qualified Pukeko.Language.KindChecker    as KindChecker
import qualified Pukeko.Language.LambdaLifter   as LambdaLifter
import qualified Pukeko.Language.Parser         as Parser
import qualified Pukeko.Language.PatternMatcher as PatternMatcher
import qualified Pukeko.Language.Inferencer     as Inferencer
import qualified Pukeko.Language.TypeChecker    as TypeChecker
import qualified Pukeko.Language.TypeResolver   as TypeResolver
import qualified Pukeko.Language.FunResolver    as FunResolver

compileToCore
  :: forall m. MonadError String m
  => Bool
  -> Parser.Module
  -> m (CoreCompiler.Module, Module LambdaLifter.Out, Module Inferencer.Out)
compileToCore unsafe module_pu = do
  let typeChecked ::
        Typed st2 => (Module st1 -> m (Module st2)) -> Module st1 -> m (Module st2)
      typeChecked act = act >=> if unsafe then pure else TypeChecker.checkModule
  module_ti <- pure module_pu
               >>= Renamer.renameModule
               >>= TypeResolver.resolveModule
               >>= FunResolver.resolveModule
               >>= KindChecker.checkModule
               >>= typeChecked Inferencer.inferModule
  module_ll <- pure module_ti
               >>= typeChecked PatternMatcher.compileModule
               >>= typeChecked (pure . LambdaLifter.liftModule)
               >>= typeChecked (pure . DeadCode.cleanModule)
  let module_cc = CoreCompiler.compileModule module_ll
  return (module_cc, module_ll, module_ti)
