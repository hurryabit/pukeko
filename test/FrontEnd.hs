module FrontEnd where

import Pukeko.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden

import           Pukeko.AST.Name
import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.FrontEnd        as FrontEnd
import           Pukeko.Pretty

diffCmd :: FilePath -> FilePath -> [String]
diffCmd ref new = ["diff", "--rcs", ref, new]

fileTest :: FilePath -> TestTree
fileTest file = goldenVsStringDiff file diffCmd (file -<.> "golden") $ do
  module0 <- runM . runNameSource . runError $
    Parser.parsePackage file >>= FrontEnd.run False
  let text = case module0 of
        Left err -> renderFailure err
        Right module1 -> render False (pretty module1)
  pure (BSL.pack text <> "\n")

allTests :: IO TestTree
allTests = do
  files <- findByExtension [".pu"] ("test" </> "frontend")
  pure $ testGroup "frontend" (map fileTest files)

main :: IO ()
main = allTests >>= defaultMain
