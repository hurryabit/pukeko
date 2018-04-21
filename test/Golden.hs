-- FIXME: There was plenty of copy-paste going on here. Remove the duplication!
module Golden where

import Pukeko.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Directory.Extra
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden

import           Pukeko.AST.Name
import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.FrontEnd        as FrontEnd
import qualified Pukeko.MiddleEnd       as MiddleEnd
import qualified Pukeko.BackEnd         as BackEnd
import           Pukeko.Pretty

pu :: String
pu = ".pu"

diffCmd :: FilePath -> FilePath -> [String]
diffCmd ref new = ["diff", "-u", ref, new]

frontEndTest :: (FilePath -> FilePath) -> FilePath -> TestTree
frontEndTest golden file = goldenVsStringDiff file diffCmd (golden file) $ do
  module0 <- runM . runNameSource . runError $
    Parser.parsePackage file >>= FrontEnd.run False
  let text = case module0 of
        Left err -> renderFailure err
        Right module1 -> render False (pretty module1)
  pure (BSL.pack text <> "\n")

middleEndTest :: (FilePath -> FilePath) -> FilePath -> TestTree
middleEndTest golden file = goldenVsStringDiff file diffCmd (golden file) $ do
  module0 <- runM . runNameSource . runError $
    Parser.parsePackage file
    >>= FrontEnd.run False
    >>= MiddleEnd.run MiddleEnd.defaultConfig
  let text = case module0 of
        Left err -> renderFailure err
        Right (module1, _) -> render False (pretty module1)
  pure (BSL.pack text <> "\n")

backEndTest :: (FilePath -> FilePath) -> FilePath -> TestTree
backEndTest golden file = goldenVsStringDiff file diffCmd (golden file) $ do
  module0 <- runM . runNameSource . runError $
    Parser.parsePackage file
    >>= FrontEnd.run False
    >>= MiddleEnd.run MiddleEnd.defaultConfig
    >>= BackEnd.run . snd
  let text = case module0 of
        Left err -> renderFailure err
        Right module1 -> render False (pretty module1)
  pure (BSL.pack text)

goldenSnippet :: FilePath -> FilePath
goldenSnippet file = file -<.> "golden"

goldenExample :: String -> FilePath -> FilePath
goldenExample comp file =
  "test" </> comp </> "examples" </> goldenSnippet (takeFileName file)

allTests :: IO TestTree
allTests = do
  frontEndSnippets  <- findByExtension [pu] ("test" </> "frontend")
  middleEndSnippets <- findByExtension [pu] ("test" </> "middleend")
  backEndSnippets   <- findByExtension [pu] ("test" </> "backend")
  examples <- filter ((pu ==) . takeExtension) <$> listFiles "examples"
  pure $
    testGroup "golden"
    [ testGroup "frontend"
      [ testGroup "snippets" $ map (frontEndTest goldenSnippet) frontEndSnippets
      , testGroup "examples" $ map (frontEndTest (goldenExample "frontend")) examples
      ]
    , testGroup "middleend"
      [ testGroup "snippets" $ map (middleEndTest goldenSnippet) middleEndSnippets
      , testGroup "examples" $ map (middleEndTest (goldenExample "middleend")) examples
      ]
    , testGroup "backend"
      [ testGroup "snippets" $ map (backEndTest goldenSnippet) backEndSnippets
      , testGroup "examples" $ map (backEndTest (goldenExample "backend")) examples
      ]
    ]

main :: IO ()
main = allTests >>= defaultMain
