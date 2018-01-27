module Pukeko.FrontEnd.Parser.Build
  ( build
  ) where

import Pukeko.Prelude

import qualified Data.DList as DL
import qualified Data.Set   as Set

import           Pukeko.AST.Surface

build ::
  (Member (Error Doc) effs) =>
  (FilePath -> Eff effs Module) -> FilePath -> Eff effs Package
build parse file = do
  (_, mdls) <-
    evalState @(Set FilePath) mempty
    $ runWriter @(DList Module)
    $ runReader @(Set FilePath) mempty
    $ go (raise . raise . raise . parse) file
  pure (MkPackage file (toList mdls))
  where
    go parse file = do
      cyc <- asks (file `Set.member`)
      when cyc $ throwError ("detected import cycle at file" <+> text file)
      done <- gets (file `Set.member`)
      unless done $ do
        mdl <- parse file
        modify (file `Set.insert`)
        local (file `Set.insert`) $ for_ (_mod2imports mdl) (go parse)
        tell (DL.singleton mdl)
