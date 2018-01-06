module Main where

import Control.Monad
import Data.Monoid
import Options.Applicative
import System.FilePath
import System.Exit

import Pukeko.Pretty

import qualified Pukeko
import qualified Pukeko.Language.Parser   as Parser
import qualified Pukeko.GMachine.Compiler as Compiler
import qualified Pukeko.GMachine.NASM     as NASM
import qualified Pukeko.GMachine.PeepHole as PeepHole

compile :: Bool -> Bool -> Bool -> Bool -> String -> IO ()
compile write_ll write_ti write_gm no_prelude file_user = do
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
        let module_ = mod_prel ++ mod_user
        (module_cc, module_ll, module_ti) <- Pukeko.compileToCore module_
        program <- Compiler.compile module_cc
        program <- pure $ PeepHole.optimize program
        nasm <- NASM.assemble program
        return (module_ll, module_ti, module_cc, program, nasm)
  case gprog_or_error of
    Left error -> do
      putStrLn $ "Error: " ++ error
      exitWith (ExitFailure 1)
    Right (Pukeko.MkModule _ module_ll, Pukeko.MkModule _ module_ti, module_cc, program, nasm) -> do
      when write_ll $ do
        writeFile (file_user `replaceExtension` ".ll") $
          (render $ vcat $ map pretty module_ll) ++ "\n"
        writeFile (file_user `replaceExtension` ".co") $
          (render $ vcat $ map pretty module_cc) ++ "\n"
      when write_ti $
        writeFile (file_user `replaceExtension` ".ti") $
          (render $ vcat $ map pretty module_ti) ++ "\n"
      when write_gm $
        writeFile (file_user `replaceExtension` ".gm") (prettyShow program)
      writeFile (file_user `replaceExtension` ".asm") nasm
      exitWith ExitSuccess

opts :: Parser (IO ())
opts =
  compile
    <$> switch (short 'l' <> long "lifted" <> help "Write result of lambda lifter")
    <*> switch (short 't' <> long "infer"  <> help "Write result of type inference")
    <*> switch (short 'g' <> long "gcode"  <> help "Write intermediate g-machine code")
    <*> switch (short 'n' <> long "no-prelude"  <> help "Don't load the prelude")
    -- <*> option auto (short 'h' <> long "heap"  <> value 1000 <> metavar "SIZE")
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info opts idm)
