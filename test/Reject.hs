{-# LANGUAGE ImplicitParams, NamedFieldPuns #-}
import Data.Char (isSpace)
import Data.List
import GHC.Stack
import Test.Hspec
import Test.Hspec.Core.Spec
import Text.Parsec

import Pukeko (ModuleLP)
import qualified Pukeko

shouldFail :: ModuleLP -> String -> String -> Expectation
shouldFail prelude expect code = do
  let result = do
        module_ <- Pukeko.parse "<input>" code
        Pukeko.compileToCore (prelude ++ module_)
  let ?callStack = freezeCallStack emptyCallStack
  case result of
    Right _ -> expectationFailure "should fail, but succeeded"
    Left actual -> case stripPrefix "\"<input>\" " actual of
      Nothing -> expectationFailure "error does not start with \"<input>\""
      Just actual -> actual `shouldBe` expect

shouldSucceed :: ModuleLP -> String -> Expectation
shouldSucceed prelude code = do
  let result = do
        module_ <- Pukeko.parse "<input>" code
        Pukeko.compileToCore (prelude ++ module_)
  let ?callStack = freezeCallStack emptyCallStack
  case result of
    Right _ -> return ()
    Left error ->
      expectationFailure $ "should succeed, but failed: " ++ error

type Parser a = Parsec [String] ModuleLP a

line :: (String -> Maybe a) -> Parser a
line f = tokenPrim id (\pos _ _ -> incSourceLine pos 1) f

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x       = Just x
           | otherwise = Nothing

skipEmpty :: Parser ()
skipEmpty = skipMany (line $ onlyIf null)

pragma :: String -> Parser String
pragma s = dropWhile isSpace <$> line (stripPrefix ("-- " ++ s)) <* skipEmpty

atSourcePos :: SourcePos -> SpecWith a -> SpecWith a
atSourcePos pos =
  let itemLocation = Just $ Location
        { locationFile     = sourceName   pos
        , locationLine     = sourceLine   pos
        , locationColumn   = sourceColumn pos
        , locationAccuracy = ExactLocation
        }
  in  mapSpecItem_ (\item -> item{itemLocation})

test :: Parser Spec
test = do
  prelude <- getState
  spcfy <- specify  <$> pragma "TEST"  <|>
           xspecify <$> pragma "XTEST"
  pos <- getPosition
  should <- shouldFail prelude <$> pragma "FAILURE" <|>
            pragma "SUCCESS" *> pure (shouldSucceed prelude)
  c <- many $ line (onlyIf (not . isPrefixOf "--"))
  return $ atSourcePos pos $ spcfy $ should (unlines c)

manySpec :: Parser Spec -> Parser Spec
manySpec p = sequence_ <$> many p

subsection :: Parser Spec
subsection = describe <$> pragma "SUBSECTION" <*> manySpec test

section :: Parser Spec
section = describe <$> pragma "SECTION" <*> manySpec subsection

spec :: Parser Spec
spec = skipEmpty *> manySpec section <* eof

main :: IO ()
main = do
  let prelFile = "test/prelude.pu"
      testFile = "test/reject.pu"
  prel <- readFile prelFile
  cont <- lines <$> readFile testFile
  prelude <- either fail return $ Pukeko.parse prelFile prel
  either (fail . show) hspec $ runParser spec prelude testFile cont
