{-# LANGUAGE RankNTypes #-}
module Pukeko.MiddleEnd
  ( Module
  , run
  )
  where

import Pukeko.Prelude hiding (run)

import qualified Pukeko.AST.NoLambda           as Lambda
import qualified Pukeko.AST.SystemF            as SystemF
import qualified Pukeko.AST.Stage              as Stage
import qualified Pukeko.FrontEnd.TypeChecker   as TypeChecker
import qualified Pukeko.MiddleEnd.AliasInliner as AliasInliner
import qualified Pukeko.MiddleEnd.TypeEraser   as TypeEraser
import qualified Pukeko.MiddleEnd.DeadCode     as DeadCode
import qualified Pukeko.MiddleEnd.EtaReducer   as EtaReducer
import qualified Pukeko.MiddleEnd.LambdaLifter as LambdaLifter
import qualified Pukeko.MiddleEnd.Prettifier   as Prettifier

type Module = Lambda.Module

run
  :: Bool
  -> SystemF.Module Stage.FrontEnd
  -> Either Doc (SystemF.Module Stage.BackEnd, Module)
run unsafe module_sf = do
  let typeChecked ::
        Stage.Typed st2 =>
        (SystemF.Module st1 -> SystemF.Module st2) ->
        (SystemF.Module st1 -> Either Doc (SystemF.Module st2))
      typeChecked f m
        | unsafe    = pure (f m)
        | otherwise = TypeChecker.checkModule (f m)
  module_ll <- pure module_sf
               >>= typeChecked LambdaLifter.liftModule
               >>= typeChecked EtaReducer.reduceModule
               >>= typeChecked AliasInliner.inlineModule
               >>= typeChecked DeadCode.cleanModule
               >>= typeChecked Prettifier.prettifyModule
  let module_lm = TypeEraser.eraseModule module_ll
  return (module_ll, module_lm)
