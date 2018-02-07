{-# LANGUAGE RankNTypes #-}
module Pukeko.MiddleEnd
  ( Module
  , Optimization (..)
  , Config (..)
  , defaultConfig
  , run
  )
  where

import Pukeko.Prelude hiding (run)

import qualified Pukeko.AST.NoLambda           as NoLambda
import qualified Pukeko.AST.SuperCore          as Core
import qualified Pukeko.AST.SystemF            as SysF
import           Pukeko.AST.Language
import qualified Pukeko.FrontEnd.TypeChecker   as TypeChecker
import qualified Pukeko.MiddleEnd.AliasInliner as AliasInliner
import qualified Pukeko.MiddleEnd.Inliner      as Inliner
import qualified Pukeko.MiddleEnd.TypeEraser   as TypeEraser
import qualified Pukeko.MiddleEnd.DeadCode     as DeadCode
import qualified Pukeko.MiddleEnd.EtaReducer   as EtaReducer
import qualified Pukeko.MiddleEnd.LambdaLifter as LambdaLifter
import qualified Pukeko.MiddleEnd.Prettifier   as Prettifier

type Module = NoLambda.Module

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
  { optimizations = [EtaReduction, AliasInlining, DeadCodeElimination, Prettification]
  , typeChecking  = True
  }

runOptimization :: Optimization -> Core.Module -> Core.Module
runOptimization = \case
  EtaReduction        -> EtaReducer.reduceModule
  AliasInlining       -> AliasInliner.inlineModule
  Inlining            -> Inliner.inlineModule
  DeadCodeElimination -> DeadCode.cleanModule
  Prettification      -> Prettifier.prettifyModule

run
  :: Config
  -> SysF.Module SystemF
  -> Either Failure (Core.Module, NoLambda.Module)
run cfg module_sf = do
  let typeChecked ::
        (Core.Module -> Core.Module) -> (Core.Module -> Either Failure Core.Module)
      typeChecked f m0 = do
        let m1 = f m0
        when (typeChecking cfg) (TypeChecker.check m1)
        pure m1
  let module_ll = LambdaLifter.liftModule module_sf
  when (typeChecking cfg) (TypeChecker.check module_ll)
  module_opt <-
    foldlM (\m opt -> typeChecked (runOptimization opt) m) module_ll (optimizations cfg)
  let module_lm = TypeEraser.eraseModule module_opt
  return (module_opt, module_lm)
