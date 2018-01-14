module Pukeko.FrontEnd.Parser.Build
  ( build
  ) where

import Pukeko.Prelude

import           Control.Lens
import qualified Data.DList as DL

import           Pukeko.AST.Surface

type BuildT = RWST (Set FilePath) (DList Module) (Set FilePath)

build :: MonadError String m => (FilePath -> m Module) -> FilePath -> m Package
build parse file = do
  (_, mdls) <- execRWST (run parse file) mempty mempty
  pure (MkPackage file (toList mdls))

run :: MonadError String m => (FilePath -> m Module) -> FilePath -> BuildT m ()
run parse file = do
  cyc <- view (contains file)
  when cyc $ throwDoc ("detected import cycle at file" <+> text file)
  done <- use (contains file)
  unless done $ do
    mdl <- lift (parse file)
    contains file .= True
    local (contains file .~ True) $ for_ (_mod2imports mdl) (run parse)
    tell (DL.singleton mdl)
