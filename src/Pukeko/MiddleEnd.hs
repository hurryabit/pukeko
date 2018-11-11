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
import qualified Pukeko.MiddleEnd.Prettifier as Prettifier
import qualified Pukeko.MiddleEnd.TypeEraser as TypeEraser

type Module = (Core.Module, NoLambda.Module)

data Optimization
  = EtaReduction
  | AliasInlining
  | Inlining
  | DeadCodeElimination
  | Prettification

data Config = Config
  { optimizations :: [Optimization]
  , typeChecking  :: Bool
  }

defaultConfig :: Config
defaultConfig = Config
  { optimizations = [EtaReduction, Inlining, Inlining, Inlining, DeadCodeElimination, Prettification]
  , typeChecking  = True
  }

runOptimization :: Member NameSource effs => Optimization -> Core.Module -> Eff effs Core.Module
runOptimization = \case
  EtaReduction        -> pure . EtaReducer.reduceModule
  AliasInlining       -> pure . AliasInliner.inlineModule
  Inlining            -> Inliner.inlineModule
  DeadCodeElimination -> pure . DeadCode.cleanModule
  Prettification      -> pure . Prettifier.prettifyModule

run :: forall effs. Members [NameSource, Error Failure] effs =>
  Config -> SysF.Module SystemF -> Eff effs Module
run cfg module_sf = do
  let typeChecked ::
        (Core.Module -> Eff effs Core.Module) -> (Core.Module -> Eff effs Core.Module)
      typeChecked f m0 = do
        m1 <- f m0
        when (typeChecking cfg) (TypeChecker.check m1)
        pure m1
  module_ll <- LambdaLifter.liftModule module_sf
  when (typeChecking cfg) (TypeChecker.check module_ll)
  module_opt <-
    foldlM (\m opt -> typeChecked (runOptimization opt) m) module_ll (optimizations cfg)
  let module_lm = TypeEraser.eraseModule module_opt
  return (module_opt, module_lm)
