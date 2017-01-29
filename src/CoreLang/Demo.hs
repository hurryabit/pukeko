module CoreLang.Demo where

import CoreLang.Language.Syntax (Expr)

import qualified CoreLang.Language.Parser         as Parser
import qualified CoreLang.Monomorphic.Checker     as Mono.Checker
import qualified CoreLang.Monomorphic.Inferrer    as Mono.Inferrer
import qualified CoreLang.Polymorphic.TypeChecker as Poly.Inferrer


polyInfer :: String -> IO ()
polyInfer = onInput Poly.Inferrer.inferExpr

polyInferFile :: String -> IO ()
polyInferFile = onFile Poly.Inferrer.inferExpr


monoCheck :: String -> IO ()
monoCheck = onInput Mono.Checker.checkExpr

monoCheckFile :: String -> IO ()
monoCheckFile = onFile Mono.Checker.checkExpr


monoInfer :: String -> IO ()
monoInfer = onInput Mono.Inferrer.inferExpr

monoInferFile :: String -> IO ()
monoInferFile = onFile Mono.Inferrer.inferExpr


onLabeledInput :: Show a => (Expr -> Either String a) -> String -> String -> IO ()
onLabeledInput f file code = do
  case Parser.parseExpr file code >>= f of
    Left  e -> putStrLn $ "error: "  ++ e
    Right t -> putStrLn $ show t

onInput :: Show a => (Expr -> Either String a) -> String -> IO ()
onInput f = onLabeledInput f "<input>"

onFile ::  Show a => (Expr -> Either String a) -> String -> IO ()
onFile f file = readFile file >>= onLabeledInput f file

code_foldl =
  "letrec foldl = fun f y0 xs -> case_list xs y0 (fun x xs -> foldl f (f y0 x) xs) in foldl"
