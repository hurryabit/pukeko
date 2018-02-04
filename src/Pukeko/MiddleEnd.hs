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
import qualified Pukeko.AST.SystemF            as SystemF
import qualified Pukeko.AST.Stage              as Stage
import qualified Pukeko.FrontEnd.TypeChecker   as TypeChecker
import qualified Pukeko.MiddleEnd.AliasInliner as AliasInliner
import qualified Pukeko.MiddleEnd.TypeEraser   as TypeEraser
import qualified Pukeko.MiddleEnd.DeadCode     as DeadCode
import qualified Pukeko.MiddleEnd.EtaReducer   as EtaReducer
import qualified Pukeko.MiddleEnd.LambdaLifter as LambdaLifter
import qualified Pukeko.MiddleEnd.Prettifier   as Prettifier

type Module = NoLambda.Module

type ModuleOpt = SystemF.Module Stage.BackEnd

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

runOptimization :: Optimization -> ModuleOpt -> ModuleOpt
runOptimization = \case
  EtaReduction        -> EtaReducer.reduceModule
  AliasInlining       -> AliasInliner.inlineModule
  DeadCodeElimination -> DeadCode.cleanModule
  Prettification      -> Prettifier.prettifyModule

run
  :: Config
  -> SystemF.Module Stage.FrontEnd
  -> Either Failure (ModuleOpt, Module)
run cfg module_sf = do
  let typeChecked ::
        Stage.Typed st2 =>
        (SystemF.Module st1 -> SystemF.Module st2) ->
        (SystemF.Module st1 -> Either Failure (SystemF.Module st2))
      typeChecked f m
        | typeChecking cfg = TypeChecker.checkModule (f m)
        | otherwise        = pure (f m)
  module_ll <- typeChecked LambdaLifter.liftModule module_sf
  module_opt <-
    foldlM (\m opt -> typeChecked (runOptimization opt) m) module_ll (optimizations cfg)
  let module_lm = TypeEraser.eraseModule module_opt
  return (module_opt, module_lm)
