{-# LANGUAGE GADTs #-}
module CoreLang.Demo where

import Control.Monad.Except
-- import Data.Set (Set)
import Text.Parsec (SourcePos)
import System.Console.Haskeline

import CoreLang.GMachine.Compiler (GProg)
import CoreLang.Language.Syntax
import CoreLang.Pretty

import qualified CoreLang.GMachine.Compiler     as Compiler
import qualified CoreLang.Language.Builtins     as Builtins
import qualified CoreLang.Language.LambdaLifter as Lifter
import qualified CoreLang.Language.Parser       as Parser
import qualified CoreLang.Language.Type         as Type
import qualified CoreLang.Language.TypeChecker  as Poly

repl :: Pretty t => (Expr SourcePos -> Either String t) -> IO ()
repl cmd = runInputT (defaultSettings { historyFile = Just ".history" }) loop
  where
    loop :: InputT IO ()
    loop = do
      input <- getInputLine "[CoReLaNG] "
      case input of
        Nothing -> return ()
        Just code -> do
          case Parser.parseExpr "<repl>" code >>= cmd of
            Left  e -> outputStrLn $ "ERROR: " ++ e
            Right t -> do
              outputStrLn (replicate 60 '=')
              outputStrLn $ prettyShow t
              outputStrLn (replicate 60 '=')
          loop

onLabeledInput :: Pretty t => (Expr SourcePos -> Either String t) -> String -> String -> IO ()
onLabeledInput f file code =
  case Parser.parseExpr file code >>= f of
    Left  e -> putStrLn $ "error: " ++ e
    Right t -> putStrLn $ prettyShow t

onInput :: Pretty t => (Expr SourcePos -> Either String t) -> String -> IO ()
onInput f = onLabeledInput f "<input>"

onFile ::  Pretty t => (Expr SourcePos -> Either String t) -> String -> IO ()
onFile f file = readFile file >>= onLabeledInput f file

liftExpr :: Expr SourcePos -> Either String GProg
liftExpr expr = do
  t1 <- Poly.inferExpr expr
  let lifted_expr = Lifter.liftExpr (map fst Builtins.everything) expr
  _ <- (Poly.inferExpr lifted_expr >>= Type.unify mempty t1)
        `catchError` \msg -> throwError (prettyShow lifted_expr ++ '\n':msg)
  prog <- Compiler.compile (fmap (const ()) lifted_expr)
  return prog

data Command where
  Command :: Pretty t => String -> (Expr SourcePos -> Either String t) -> Command

debug :: (Show t, Pretty t) => (Expr SourcePos -> Either String t)
                            -> (Expr SourcePos -> Either String (Debug t))
debug action expr = do
  res <- action expr
  let pre  = text $ show $ fmap (const ()) expr
      post = text $ show $ res
      doc = vcat
        [ pre
        , text (replicate 60 '-')
        , pretty res
        , text (replicate 60 '-')
        , post
        ]
  return (Debug doc)


newtype Debug t = Debug Doc

instance Pretty (Debug t) where
  pPrint (Debug doc) = doc
