module Main where

import Control.Monad
import Control.Monad.Except
import Options.Applicative
import System.Exit

import CoreLang.Demo


-- opts :: Parser (IO ())
-- opts =
--   interpreter 
--     <$> option auto (short 'h' <> long "heap" <> value 100 <> metavar "SIZE")
--     <*> argument str (metavar "FILE")

main :: IO ()
main = repl lambdaLifter
