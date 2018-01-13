module Main where

import Control.Monad
import Data.Monoid
import Options.Applicative
import System.FilePath
import System.Exit

import Pukeko.Pretty

import qualified Pukeko.FrontEnd.Parser as Parser
import qualified Pukeko.FrontEnd        as FrontEnd
import qualified Pukeko.MiddleEnd       as MiddleEnd
import qualified Pukeko.BackEnd         as BackEnd

compile :: Bool -> Bool -> Bool -> Bool -> Bool -> String -> IO ()
compile write_ll write_sf _write_gm no_prelude unsafe file_user = do
  let file_prel = replaceFileName file_user "prelude.pu"
  code_user <- readFile file_user
  code_prel <-
    if no_prelude
    then return ""
    else readFile file_prel
  let gprog_or_error = do
        mod_user <- Parser.parseModule file_user code_user
        mod_prel <-
          if no_prelude
          then return []
          else Parser.parseModule file_prel code_prel
        let module_pu = mod_prel ++ mod_user
        module_sf <- FrontEnd.run module_pu
        (module_ll, module_lm) <- MiddleEnd.run unsafe module_sf
        nasm <- BackEnd.run module_lm
        return (module_sf, module_ll, nasm)
  case gprog_or_error of
    Left error -> do
      putStrLn $ "Error: " ++ error
      exitWith (ExitFailure 1)
    Right (module_sf, module_ll, nasm) -> do
      when write_ll $ do
        writeFile (file_user `replaceExtension` ".ll") $
          render (pPrintPrec prettyNormal 0 module_ll) ++ "\n"
      when write_sf $
        writeFile (file_user `replaceExtension` ".ti") $
          render (pPrintPrec prettyNormal 0 module_sf) ++ "\n"
      writeFile (file_user `replaceExtension` ".asm") nasm
      exitWith ExitSuccess

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
