module Pukeko.FrontEnd.Parser.Build
  ( build
  ) where

import Pukeko.Prelude

import qualified Data.DList as DL
import qualified Data.Set as Set
import           System.FilePath

import Pukeko.AST.Surface

build ::
  (Member (Error Failure) effs) =>
  (FilePath -> Eff effs Module) -> FilePath -> Eff effs Package
build parse rootFile = do
  (_, mdls) <-
    evalState @(Set FilePath) mempty
    $ runWriter @(DList Module)
    $ runReader @(Set FilePath) mempty
    $ go rootFile
  pure (MkPackage rootFile (toList mdls))
  where
    go file = do
      cyc <- asks (file `Set.member`)
      when cyc $ throwFailure ("detected import cycle at file" <+> pretty file)
      done <- gets (file `Set.member`)
      unless done $ do
        mdl <- raise . raise . raise $ parse file
        modify (file `Set.insert`)
        local (file `Set.insert`) $ for_ (_mod2imports mdl) $ \imp ->
          go (takeDirectory rootFile </> imp <.> "pu")
        tell (DL.singleton mdl)
