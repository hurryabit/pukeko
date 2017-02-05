module Main where

import Control.Monad
import Control.Monad.Except
import Data.Monoid
import Options.Applicative
import System.Exit

import CoreLang.Demo

lifter :: Int -> String -> IO ()
lifter _ = onFile lazyLifter

opts :: Parser (IO ())
opts =
  lifter
    <$> option auto (short 'h' <> long "heap" <> value 100 <> metavar "SIZE")
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info opts idm)
