module Main where

import Pukeko.Prelude

import Options.Applicative
import System.FilePath ((-<.>))
import System.Exit

import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.FrontEnd        as FrontEnd
import qualified Pukeko.MiddleEnd       as MiddleEnd
import qualified Pukeko.BackEnd         as BackEnd

unlift :: (Monad m) => Either e a -> Eff [Error e, m] a
unlift = either throwError pure

compile :: Bool -> Bool -> Bool -> String -> IO ()
compile write_pl stop_tc unsafe file = do
  ok_or_error <- runM . runError $ do
    package <- Parser.parsePackage file
    module_sf <- unlift (FrontEnd.run unsafe package)
    if stop_tc
      then do
        sendM $ writeFile (file -<.> "ti")
          (render (pretty module_sf) ++ "\n")
      else do
        (module_pl, module_lm) <- unlift (MiddleEnd.run unsafe module_sf)
        nasm <- unlift (BackEnd.run module_lm)
        sendM $ do
          when write_pl $ do
            writeFile (file -<.> "pl") $
              render (pretty module_pl) ++ "\n"
          writeFile (file -<.> "asm") nasm
  case ok_or_error of
    Left err -> do
      putStrLn $ "Error: " ++ render err
      exitWith (ExitFailure 1)
    Right () -> exitWith ExitSuccess

opts :: Parser (IO ())
opts =
  compile
    <$> switch (short 'l' <> help "Write result of lambda lifter")
    <*> switch (short 't' <> help "Stop after type checking")
    <*> switch (short 'u' <> help "Don't run the type checker")
    -- <*> option auto (short 'h' <> long "heap"  <> value 1000 <> metavar "SIZE")
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info opts idm)
