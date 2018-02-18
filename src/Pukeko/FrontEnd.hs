module Pukeko.FrontEnd
  ( Module
  , run
  )
  where

import Pukeko.Prelude hiding (run)

import qualified Pukeko.AST.SystemF             as SystemF
import           Pukeko.AST.Language
import qualified Pukeko.FrontEnd.ClassEliminator as ClassEliminator
import qualified Pukeko.FrontEnd.Renamer        as Renamer
import qualified Pukeko.FrontEnd.KindChecker    as KindChecker
import qualified Pukeko.FrontEnd.Parser         as Parser
import qualified Pukeko.FrontEnd.PatternMatcher as PatternMatcher
import qualified Pukeko.FrontEnd.Inferencer     as Inferencer
import qualified Pukeko.FrontEnd.TypeChecker    as TypeChecker
import qualified Pukeko.FrontEnd.FunResolver    as FunResolver

type Module = SystemF.Module SystemF

run :: Member (Error Failure) effs => Bool -> Parser.Package -> Eff effs Module
run unsafe pkg = do
  surface <- Renamer.renameModule pkg
  FunResolver.resolveModule  surface
  KindChecker.checkModule    surface
  typed <- Inferencer.inferModule surface
  TypeChecker.check typed
  unnested <- PatternMatcher.compileModule typed
  TypeChecker.check unnested
  let unclassy = ClassEliminator.elimModule unnested
  unless unsafe (TypeChecker.check unclassy)
  pure unclassy
