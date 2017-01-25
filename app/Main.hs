module Main where

import Control.Monad
import Control.Monad.Except
import Options.Applicative
import System.Exit

import CoreLang.Language.Parser (parseProgram)
import CoreLang.Language.TypeChecker (checkProgram)
import CoreLang.TempInst.Interpreter (interpretProgram)

interpreter :: Int -> String -> IO ()
interpreter heapSize file = do
  res <- runExceptT $ do
    code <- liftIO (readFile file)
    program <- parseProgram file code
    checkProgram program
    interpretProgram heapSize program
  case res of
    Left error         -> do
      putStrLn ("error: " ++ error)
      exitFailure
    Right (num, stats) -> do
      putStrLn stats
      let exitCode =
            if num == 0 then ExitSuccess else ExitFailure (fromInteger num)
      exitWith exitCode

opts :: Parser (IO ())
opts =
  interpreter 
    <$> option auto (short 'h' <> long "heap" <> value 100 <> metavar "SIZE")
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info opts idm)
