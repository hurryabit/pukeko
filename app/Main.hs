module Main where

import Control.Monad
import Data.Monoid
import Options.Applicative
import System.FilePath

import CoreLang.Pretty
import qualified CoreLang.GMachine.Compiler       as Compiler
import qualified CoreLang.GMachine.Machine        as GMachine
import qualified CoreLang.Language.LambdaLifter   as Lifter
import qualified CoreLang.Language.Parser         as Parser
import qualified CoreLang.Language.Type           as Type
import qualified CoreLang.Monomorphic.Checker     as Mono
import qualified CoreLang.Polymorphic.TypeChecker as Poly
import qualified CoreLang.Polymorphic.Builtins    as Builtins


doit :: Int -> Int -> String -> IO ()
doit stack heap file = do
  code <- readFile file
  let gprog_or_error = do
        expr <- Parser.parseExpr file code
        _ <- Poly.inferExpr expr
        let lifted_expr = Lifter.liftExpr (map fst Builtins.everything) expr
        gprog <- Compiler.compile (fmap (const ()) lifted_expr)
        return (lifted_expr, gprog)
  case gprog_or_error of
    Left error -> putStrLn $ "Error: " ++ error
    Right (lifted_expr, gprog) -> do
      writeFile (file `replaceExtension` ".ll") (prettyShow lifted_expr)
      writeFile (file `replaceExtension` ".gm") (prettyShow gprog)
      GMachine.execute gprog stack heap

opts :: Parser (IO ())
opts =
  doit
    <$> option auto (short 's' <> long "stack" <> value 1000 <> metavar "SIZE")
    <*> option auto (short 'h' <> long "heap"  <> value 1000 <> metavar "SIZE")
    <*> argument str (metavar "FILE")

main :: IO ()
main = join $ execParser (info opts idm)
