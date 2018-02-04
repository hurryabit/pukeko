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
import qualified Pukeko.AST.SuperCore          as SC
import           Pukeko.AST.Language
import qualified Pukeko.FrontEnd.TypeChecker   as TypeChecker
import qualified Pukeko.MiddleEnd.AliasInliner as AliasInliner
import qualified Pukeko.MiddleEnd.TypeEraser   as TypeEraser
import qualified Pukeko.MiddleEnd.DeadCode     as DeadCode
import qualified Pukeko.MiddleEnd.EtaReducer   as EtaReducer
import qualified Pukeko.MiddleEnd.LambdaLifter as LambdaLifter
import qualified Pukeko.MiddleEnd.Prettifier   as Prettifier

type Module = NoLambda.Module

type ModuleSC = SC.ModuleSC

data Optimization
  = EtaReduction
  | AliasInlining
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

runOptimization :: Optimization -> ModuleSC -> ModuleSC
runOptimization = \case
  EtaReduction        -> EtaReducer.reduceModule
  AliasInlining       -> AliasInliner.inlineModule
  DeadCodeElimination -> DeadCode.cleanModule
  Prettification      -> Prettifier.prettifyModule

run
  :: Config
  -> SC.Module SystemF
  -> Either Failure (ModuleSC, Module)
run cfg module_sf = do
  let typeChecked :: (ModuleSC -> ModuleSC) -> (ModuleSC -> Either Failure ModuleSC)
      typeChecked f m0 = do
        let m1 = f m0
        when (typeChecking cfg) (TypeChecker.checkModule (SC.toModule m1))
        pure m1
  let module_ll = LambdaLifter.liftModule module_sf
  when (typeChecking cfg) (TypeChecker.checkModule (SC.toModule module_ll))
  module_opt <-
    foldlM (\m opt -> typeChecked (runOptimization opt) m) module_ll (optimizations cfg)
  let module_lm = TypeEraser.eraseModule module_opt
  return (module_opt, module_lm)
