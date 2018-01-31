module Pukeko.FrontEnd.Parser.Build
  ( build
  ) where

import Pukeko.Prelude

import qualified Data.DList as DL
import qualified Data.Set   as Set

import           Pukeko.AST.Surface

build ::
  (Member (Error Failure) effs) =>
  (FilePath -> Eff effs Module) -> FilePath -> Eff effs Package
build parse file = do
  (_, mdls) <-
    evalState @(Set FilePath) mempty
    $ runWriter @(DList Module)
    $ runReader @(Set FilePath) mempty
    $ go parse file
  pure (MkPackage file (toList mdls))
  where
    go parse file = do
      cyc <- asks (file `Set.member`)
      when cyc $ raise3 (throwFailure ("detected import cycle at file" <+> pretty file))
      done <- gets (file `Set.member`)
      unless done $ do
        mdl <- raise3 (parse file)
        modify (file `Set.insert`)
        local (file `Set.insert`) $ for_ (_mod2imports mdl) (go parse)
        tell (DL.singleton mdl)
    raise3 = raise . raise . raise
