module Main where

import Pukeko.Prelude

import Options.Applicative
import System.FilePath ((-<.>))
import System.Exit

import Pukeko.Pretty

import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.FrontEnd        as FrontEnd
import qualified Pukeko.MiddleEnd       as MiddleEnd
import qualified Pukeko.BackEnd         as BackEnd

compile :: Bool -> Bool -> Bool -> Bool -> Bool -> String -> IO ()
compile write_pl write_sf _write_gm _no_prelude unsafe file = do
  ok_or_error <- runExceptT $ do
    package <- Parser.parsePackage file
    module_sf <- FrontEnd.run package
    (module_pl, module_lm) <- MiddleEnd.run unsafe module_sf
    nasm <- BackEnd.run module_lm
    liftIO $ do
      when write_pl $ do
        writeFile (file -<.> "pl") $
          render (pPrintPrec prettyNormal 0 module_pl) ++ "\n"
      when write_sf $
        writeFile (file -<.> "ti") $
        render (pPrintPrec prettyNormal 0 module_sf) ++ "\n"
      writeFile (file -<.> "asm") nasm
  case ok_or_error of
    Left error -> do
      putStrLn $ "Error: " ++ error
      exitWith (ExitFailure 1)
    Right () -> exitWith ExitSuccess

opts :: Parser (IO ())
opts =
  compile
    <$> switch (short 'l' <> long "lifted" <> help "Write result of lambda lifter")
    <*> switch (short 't' <> long "infer"  <> help "Write result of type inference")
    <*> switch (short 'g' <> long "gcode"  <> help "Write intermediate g-machine code")
    <*> switch (short 'n' <> long "no-prelude"  <> help "Don't load the prelude")
    <*> switch (short 'u' <> long "unsafe" <> help "Don't run the type checker")
    -- <*> option auto (short 'h' <> long "heap"  <> value 1000 <> metavar "SIZE")
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info opts idm)
