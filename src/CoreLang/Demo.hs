module CoreLang.Demo where

import Text.Printf

import qualified CoreLang.Language.Parser         as Parser
import qualified CoreLang.Monomorphic.TypeChecker as MonoChecker
import qualified CoreLang.Polymorphic.TypeChecker as PolyChecker


inferExpr :: String -> IO ()
inferExpr code = do
  case Parser.parseExpr code >>= PolyChecker.inferExpr of
    Left  e -> printf "Error = %s\n" e
    Right t -> print t


checkFile :: String -> IO ()
checkFile file = do
  code <- readFile file
  case Parser.parseProgram file code >>= PolyChecker.checkProgram of
    Left  e  -> printf "Error = %s\n" e
    Right () -> putStrLn "OK"

monoInferExpr :: String -> IO ()
monoInferExpr code = do
  case Parser.parseExpr code >>= MonoChecker.inferExpr of
    Left  e -> printf "Error = %s\n" e
    Right t -> print t


monoCheckFile :: String -> IO ()
monoCheckFile file = do
  code <- readFile file
  case Parser.parseProgram file code >>= MonoChecker.checkProgram of
    Left  e  -> printf "Error = %s\n" e
    Right () -> putStrLn "OK"


code_foldl =
  "letrec foldl = fun f y0 xs -> case_list xs y0 (fun x xs -> foldl f (f y0 x) xs) in foldl"
