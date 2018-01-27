module GoldenFrontEnd where

import Pukeko.Prelude hiding ((<|>), many)

import Control.Monad.Trans.Class (lift)
import Data.List
import System.Directory (setCurrentDirectory)
import System.IO
import Test.Tasty
import Test.Tasty.Golden
import Text.Parsec hiding (Error)

import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.FrontEnd        as FrontEnd

out :: String -> HIO ()
out s = do
  h <- ask
  sendM (hPutStr h s)

runSnippet :: Parser.Package -> String -> HIO ()
runSnippet prelude code = do
  let result = do
        module_ <- run . runError $ Parser.parseInput "<input>" code
        FrontEnd.run False (module_ `Parser.extend` prelude)
  case result of
    Right _ ->
      out "-- SUCCESS\n"
    Left actual -> case stripPrefix "\"<input>\" " (render actual) of
      Nothing -> out ("-- FAILURE FIXME " ++ render actual ++ "\n")
      Just actual -> out ("-- FAILURE " ++ actual ++ "\n")
  out code

type HIO = Eff [Reader Handle, IO]

type Parser = ParsecT [String] Parser.Package HIO

line :: (String -> Bool) -> Parser String
line p = tokenPrim id (\pos _ _ -> incSourceLine pos 1) (onlyIf p)

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x       = Just x
           | otherwise = Nothing

skipEmpty :: Parser [String]
skipEmpty = many (line null)

pragma :: String -> Parser String
pragma s = do
  l0 <- line (("-- " ++ s) `isPrefixOf`)
  ls <- skipEmpty
  pure (unlines (l0:ls))

test :: Parser ()
test = do
  prelude <- getState
  t <- pragma "TEST"  <|> pragma "XTEST"
  _ <- pragma "FAILURE" <|> pragma "SUCCESS"
  c <- many $ line (not . isPrefixOf "--")
  lift $ do
    out t
    runSnippet prelude (unlines c)

manySpec :: Parser () -> Parser ()
manySpec p = void (many p)

subsection :: Parser ()
subsection = do
  s <- pragma "SUBSECTION"
  lift (out s)
  manySpec test

section :: Parser ()
section = do
  s <- pragma "SECTION"
  lift (out s)
  manySpec subsection

spec :: Parser ()
spec = do
  s <- skipEmpty
  lift (out (unlines s))
  manySpec section
  eof

testFile, outFile :: FilePath
testFile = "frontend.pu"
outFile = "frontend.out"

frontEndTest :: IO ()
frontEndTest = do
  setCurrentDirectory "test"
  let prelFile = "std/prelude.pu"
  cont <- lines <$> readFile testFile
  prelude <- runM $
    interpretM (\(Error msg) -> fail (render msg)) (Parser.parsePackage prelFile)
  h <- openFile outFile WriteMode
  runM (runReader h (runParserT spec prelude testFile cont))
    >>= either (fail . show) pure
  hClose h

main :: IO ()
main = defaultMain (goldenVsFile "front end test" testFile outFile frontEndTest)
