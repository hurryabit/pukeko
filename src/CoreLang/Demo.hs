module CoreLang.Demo where

import Text.Printf

import qualified CoreLang.Language.Parser         as Parser
import qualified CoreLang.Monomorphic.TypeChecker as Mono.Checker
import qualified CoreLang.Polymorphic.TypeChecker as Poly.Checker


polyInfer :: String -> IO ()
polyInfer = polyInferIO "<interactive>"

polyInferFile :: String -> IO ()
polyInferFile file = readFile file >>= polyInferIO file

polyInferIO :: String -> String -> IO ()
polyInferIO file code = do
  case Parser.parseExpr file code >>= Poly.Checker.inferExpr of
    Left  e -> printf "Error = %s\n" e
    Right t -> printf "Type = %s\n" (show t)

monoInfer :: String -> IO ()
monoInfer = monoInferIO "<interactive>"

monoInferFile :: String -> IO ()
monoInferFile file = readFile file >>= monoInferIO file

monoInferIO :: String -> String -> IO ()
monoInferIO file code = do
  case Parser.parseExpr file code >>= Mono.Checker.inferExpr of
    Left  e -> printf "Error = %s\n" e
    Right t -> printf "Type = %s\n" (show t)

code_foldl =
  "letrec foldl = fun f y0 xs -> case_list xs y0 (fun x xs -> foldl f (f y0 x) xs) in foldl"
