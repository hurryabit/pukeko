module Main where

import Control.Monad
import Data.Monoid
import Options.Applicative
import System.FilePath
import System.Exit

import CoreLang.Pretty

import qualified CoreLang.GMachine.Compiler     as Compiler
import qualified CoreLang.GMachine.NASM         as NASM
import qualified CoreLang.Language.Builtins     as Builtins
import qualified CoreLang.Language.LambdaLifter as Lifter
import qualified CoreLang.Language.Parser       as Parser
import qualified CoreLang.Language.Type         as Type
import qualified CoreLang.Language.TypeChecker  as Poly

compile :: Bool -> String -> IO ()
compile write_ll file = do
  code <- readFile file
  let gprog_or_error = do
        expr <- Parser.parseExpr file code
        t <- Poly.inferExpr expr
        _ <- Type.unify mempty t (Type.int)
        let lifted_expr = Lifter.liftExpr (map fst Builtins.everything) expr
        program <- Compiler.compile (fmap (const ()) lifted_expr)
        nasm <- NASM.assemble program
        return (lifted_expr, nasm)
  case gprog_or_error of
    Left error -> do
      putStrLn $ "Error: " ++ error
      exitWith (ExitFailure 1)
    Right (lifted_expr, nasm) -> do
      when write_ll $
        writeFile (file `replaceExtension` ".ll") (prettyShow lifted_expr)
      writeFile (file `replaceExtension` ".asm") nasm
      exitWith ExitSuccess

opts :: Parser (IO ())
opts =
  compile
    <$> switch (short 'l' <> long "lifted" <> help "Write result of lambda lifter")
    -- <*> option auto (short 'h' <> long "heap"  <> value 1000 <> metavar "SIZE")
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info opts idm)
