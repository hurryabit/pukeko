{-# LANGUAGE RankNTypes #-}
module Pukeko.FrontEnd
  ( Module
  , run
  )
  where

import Pukeko.Prelude hiding (run)

import qualified Pukeko.AST.SystemF             as SystemF
import qualified Pukeko.AST.Stage               as Stage
import qualified Pukeko.FrontEnd.ClassEliminator as ClassEliminator
import qualified Pukeko.FrontEnd.Renamer        as Renamer
import qualified Pukeko.FrontEnd.KindChecker    as KindChecker
import qualified Pukeko.FrontEnd.Parser         as Parser
import qualified Pukeko.FrontEnd.PatternMatcher as PatternMatcher
import qualified Pukeko.FrontEnd.Inferencer     as Inferencer
import qualified Pukeko.FrontEnd.TypeChecker    as TypeChecker
import qualified Pukeko.FrontEnd.TypeResolver   as TypeResolver
import qualified Pukeko.FrontEnd.FunResolver    as FunResolver

type Module = SystemF.Module Stage.ClassEliminator

run :: Bool -> Parser.Package -> Either Failure Module
run unsafe =
  Renamer.renameModule
  >=> TypeResolver.resolveModule
  >=> FunResolver.resolveModule
  >=> KindChecker.checkModule
  >=> Inferencer.inferModule
  >=> TypeChecker.checkModule
  >=> PatternMatcher.compileModule
  >=> TypeChecker.checkModule
  >=> pure . ClassEliminator.elimModule
  >=> if unsafe then pure else TypeChecker.checkModule
