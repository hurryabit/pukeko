module Main where

import Control.Monad
import Data.Monoid
import Options.Applicative
import System.FilePath
import System.Exit

import Pukeko.Pretty

import qualified Pukeko.GMachine.Compiler     as Compiler
import qualified Pukeko.GMachine.NASM         as NASM
import qualified Pukeko.Language.Builtins     as Builtins
import qualified Pukeko.Language.LambdaLifter as Lifter
import qualified Pukeko.Language.Parser       as Parser
import qualified Pukeko.Language.Type         as Type
import qualified Pukeko.Language.TypeChecker  as Poly

compile :: Bool -> Bool -> String -> IO ()
compile write_ll write_gm file = do
  code <- readFile file
  let gprog_or_error = do
        expr <- Parser.parseExpr file code
        t <- Poly.inferExpr expr
        _ <- Type.unify mempty t (Type.int)
        let lifted_expr = Lifter.liftExpr (map fst Builtins.everything) expr
        program <- Compiler.compile (fmap (const ()) lifted_expr)
        nasm <- NASM.assemble program
        return (lifted_expr, program, nasm)
  case gprog_or_error of
    Left error -> do
      putStrLn $ "Error: " ++ error
      exitWith (ExitFailure 1)
    Right (lifted_expr, program, nasm) -> do
      when write_ll $
        writeFile (file `replaceExtension` ".ll") (prettyShow lifted_expr)
      when write_gm $
        writeFile (file `replaceExtension` ".gm") (prettyShow program)
      writeFile (file `replaceExtension` ".asm") nasm
      exitWith ExitSuccess

opts :: Parser (IO ())
opts =
  compile
    <$> switch (short 'l' <> long "lifted" <> help "Write result of lambda lifter")
    <*> switch (short 'g' <> long "gcode"  <> help "Write intermediate g-machine code")
    -- <*> option auto (short 'h' <> long "heap"  <> value 1000 <> metavar "SIZE")
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info opts idm)
