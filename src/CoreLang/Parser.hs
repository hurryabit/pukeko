{-# LANGUAGE NamedFieldPuns #-}
module CoreLang.Parser where

import Control.Monad (msum)
import Text.Parsec as Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token

import CoreLang.Syntax

parse = Parsec.parse program

parseExpr = Parsec.parse (expr <* eof) ""

relOpNames = ["<", "<=", "==", "~=", ">=", ">"]

coreLangDef = Language.haskellStyle
  { Token.reservedNames =
      [ "let", "letrec", "in"
      , "case", "of"
      , "Pack"
      ]
  , Token.reservedOpNames = concat
      [ ["+", "-", "*", "/"]
      , relOpNames
      , ["&", "|"]
      ]
  }

Token.TokenParser
  { Token.identifier
  , Token.reserved
  , Token.reservedOp
  , Token.natural
  , Token.symbol
  , Token.parens
  , Token.braces
  , Token.angles
  , Token.comma
  , Token.dot
  , Token.semiSep1
  } =
  Token.makeTokenParser coreLangDef

equals = symbol "="

arrow = symbol "->"

lambda = symbol "\\"

type Parser = Parsec String ()

program :: Parser Program
program = semiSep1 definition <* eof

definition :: Parser Definition
definition =
  (,,) <$> identifier <*> many identifier <* equals <*> expr

localDefinition :: Parser LocalDefinition
localDefinition =
  (,) <$> identifier <* equals <*> expr

alter :: Parser Alter
alter =
  (,,)  <$> angles natural
        <*> many identifier
        <*  arrow
        <*> expr

expr, aexpr :: Parser Expr
expr = msum
  [ let isRec = msum
          [ reserved "let"    *> pure NonRecursive
          , reserved "letrec" *> pure Recursive
          ]
    in  Let <$> isRec 
            <*> semiSep1 localDefinition 
            <*  reserved "in"
            <*> expr
  , Case  <$> (reserved "case" *> expr)
          <*  reserved "of"
          <*> semiSep1 alter
  , Lam <$> (lambda *> many1 identifier)
        <*  dot
        <*> expr
  , let infixBinOp op = Infix (reservedOp op *> pure (Ap . Ap (Var op)))
    in  buildExpressionParser
          [ [ infixBinOp "*" AssocRight
            , infixBinOp "/" AssocNone
            ]
          , [ infixBinOp "+" AssocRight
            , infixBinOp "-" AssocNone
            ]
          , map (flip infixBinOp AssocNone) relOpNames
          , [infixBinOp "&" AssocRight]
          , [infixBinOp "|" AssocRight]
          ]
          (foldl1 Ap <$> many1 aexpr)
  ]
aexpr = msum
  [ Var <$> identifier
  , Num <$> natural
  , reserved "Pack" *> braces (Constr <$> natural <* comma <*> natural)
  , parens expr
  ]
