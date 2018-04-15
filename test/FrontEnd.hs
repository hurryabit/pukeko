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
diffCmd ref new = ["diff", "-u", ref, new]

fileTest :: FilePath -> TestTree
fileTest file = goldenVsStringDiff file diffCmd (file -<.> "golden") $ do
  module0 <- runM . runNameSource . runError $
    Parser.parsePackage file >>= FrontEnd.run False
  let text = case module0 of
        Left err -> renderFailure err
        Right module1 -> render False (pretty module1)
  pure (BSL.pack text <> "\n")

allTests :: IO TestTree
allTests =
  testGroup "frontend" . map fileTest
  <$> findByExtension [".pu"] ("test" </> "frontend")

main :: IO ()
main = allTests >>= defaultMain
