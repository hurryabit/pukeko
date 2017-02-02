{-# LANGUAGE GADTs #-}
module CoreLang.Demo where

import System.Console.Haskeline

import CoreLang.Language.Syntax (Expr)
import CoreLang.Pretty

import qualified CoreLang.Language.Parser         as Parser
import qualified CoreLang.Monomorphic.Checker     as Mono
-- import qualified CoreLang.Monomorphic.Inferrer    as Mono.Inferrer
import qualified CoreLang.Polymorphic.TypeChecker as Poly


repl :: Pretty a => (Expr -> Either String a) -> IO ()
repl cmd = runInputT (defaultSettings { historyFile = Just ".history" }) loop
  where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "[CoReLaNG] "
      case input of
        Nothing   -> return ()
        Just code -> do
          case Parser.parseExpr "<repl>" code >>= cmd of
            Left  e -> outputStrLn $ "ERROR: " ++ e
            Right t -> outputStrLn $ prettyShow t
          loop

onLabeledInput :: Pretty a => (Expr -> Either String a) -> String -> String -> IO ()
onLabeledInput f file code = do
  case Parser.parseExpr file code >>= f of
    Left  e -> putStrLn $ "error: " ++ e
    Right t -> putStrLn $ prettyShow t

onInput :: Pretty a => (Expr -> Either String a) -> String -> IO ()
onInput f = onLabeledInput f "<input>"

file ::  Pretty a => (Expr -> Either String a) -> String -> IO ()
file f file = readFile file >>= onLabeledInput f file

code_foldl =
  "letrec foldl = fun f y0 xs -> case_list xs y0 (fun x xs -> foldl f (f y0 x) xs) in foldl"

data Command where
  Command :: Pretty a => String -> (Expr -> Either String a) -> Command

commands :: [Command]
commands =
  [ Command "parse"      pure
  , Command "mono.check" Mono.checkExpr
  , Command "poly.infer" Poly.inferExpr
  ]
