{-# LANGUAGE RankNTypes #-}
module Pukeko.FrontEnd
  ( Module
  , run
  )
  where

import           Control.Monad ((>=>))
import           Pukeko.Error

import qualified Pukeko.AST.SystemF             as SystemF
import qualified Pukeko.AST.Stage               as Stage
import qualified Pukeko.FrontEnd.Renamer        as Renamer
import qualified Pukeko.FrontEnd.KindChecker    as KindChecker
import qualified Pukeko.FrontEnd.Parser         as Parser
import qualified Pukeko.FrontEnd.PatternMatcher as PatternMatcher
import qualified Pukeko.FrontEnd.Inferencer     as Inferencer
import qualified Pukeko.FrontEnd.TypeChecker    as TypeChecker
import qualified Pukeko.FrontEnd.TypeResolver   as TypeResolver
import qualified Pukeko.FrontEnd.FunResolver    as FunResolver

type Module = SystemF.Module Stage.PatternMatcher

run :: MonadError String m => Parser.Module -> m Module
run =
  Renamer.renameModule
  >=> TypeResolver.resolveModule
  >=> FunResolver.resolveModule
  >=> KindChecker.checkModule
  >=> Inferencer.inferModule
  >=> TypeChecker.checkModule
  >=> PatternMatcher.compileModule
  >=> TypeChecker.checkModule
