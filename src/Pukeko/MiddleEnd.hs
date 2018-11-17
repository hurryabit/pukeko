module Pukeko.MiddleEnd
  ( Module
  , Optimization (..)
  , Config (..)
  , defaultConfig
  , run
  )
  where

import Pukeko.Prelude hiding (run)

import           Pukeko.AST.Language
import           Pukeko.AST.Name
import qualified Pukeko.AST.NoLambda as NoLambda
import qualified Pukeko.AST.SuperCore as Core
import qualified Pukeko.AST.SystemF as SysF
import qualified Pukeko.FrontEnd.TypeChecker as TypeChecker
import qualified Pukeko.MiddleEnd.AliasInliner as AliasInliner
import qualified Pukeko.MiddleEnd.DeadCode as DeadCode
import qualified Pukeko.MiddleEnd.EtaReducer as EtaReducer
import qualified Pukeko.MiddleEnd.Inliner as Inliner
import qualified Pukeko.MiddleEnd.LambdaLifter as LambdaLifter
import qualified Pukeko.MiddleEnd.TypeEraser as TypeEraser

type Module = (Core.Module, NoLambda.Module)

data Optimization
  = EtaReduction
  | AliasInlining
  | Inlining

data Config = Config
  { optimizations :: [Optimization]
  , typeChecking  :: Bool
  , deadCodeElimination :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { optimizations = [EtaReduction, Inlining, Inlining, Inlining]
  , typeChecking  = True
  , deadCodeElimination = True
  }

runOptimization :: Member NameSource effs => Optimization -> Core.Module -> Eff effs Core.Module
runOptimization = \case
  EtaReduction  -> pure . EtaReducer.reduceModule
  AliasInlining -> pure . AliasInliner.inlineModule
  Inlining      -> Inliner.inlineModule

run :: forall effs. Members [NameSource, Error Failure] effs =>
  Config -> SysF.Module SystemF -> Eff effs Module
run cfg module_sf = do
    module_lifted <- LambdaLifter.liftModule module_sf
    when (typeChecking cfg) (TypeChecker.check module_lifted)
    module_opt <-
      foldlM (\m opt -> typeChecked (runOptimization opt) m) module_lifted (optimizations cfg)
    module_clean <- if deadCodeElimination cfg
      then typeChecked (pure . DeadCode.cleanModule) module_opt
      else pure module_opt
    let module_untyped = TypeEraser.eraseModule module_clean
    return (module_clean, module_untyped)
  where
    typeChecked ::
      (Core.Module -> Eff effs Core.Module) -> (Core.Module -> Eff effs Core.Module)
    typeChecked f m0 = do
      m1 <- f m0
      when (typeChecking cfg) (TypeChecker.check m1)
      pure m1
