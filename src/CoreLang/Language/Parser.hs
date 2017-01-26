module CoreLang.Language.Parser 
  ( parseExpr
  , parseProgram
  )
  where

import Control.Monad (msum)
import Control.Monad.Except
import Text.Parsec as Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token

import CoreLang.Language.Syntax


parseProgram :: MonadError String m => String -> String -> m Program
parseProgram file code =
  case Parsec.parse (program <* eof) file code of
    Left error -> throwError (show error)
    Right prog -> return prog

parseExpr :: MonadError String m => String -> m Expr
parseExpr code = 
  case Parsec.parse (expr <* eof) "<expr>" code of
    Left error -> throwError (show error)
    Right expr -> return expr

relOpNames = ["<", "<=", "==", "!=", ">=", ">"]

coreLangDef = Language.haskellStyle
  { Token.reservedNames =
      [ "fun", "let", "letrec", "and", "in"
      -- , "case", "of"
      , "Pack"
      ]
  , Token.reservedOpNames = concat
      [ ["+", "-", "*", "/"]
      , relOpNames
      , ["&&", "||"]
      , ["->"]
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
  , Token.comma
  , Token.semiSep1
  } =
  Token.makeTokenParser coreLangDef

equals = symbol "="

arrow = symbol "->"

type Parser = Parsec String ()

program :: Parser Program
program = semiSep1 definition <* eof

definition :: Parser Definition
definition =
  (,,) <$> identifier <*> many identifier <* equals <*> expr

localDefinition :: Parser LocalDefinition
localDefinition =
  (,) <$> identifier <* equals <*> expr

-- alter :: Parser Alter
-- alter =
--   (,,)  <$> angles natural
--         <*> many identifier
--         <*  arrow
--         <*> expr

expr, aexpr :: Parser Expr
expr = msum
  [ let isRec = msum
          [ reserved "let"    *> pure NonRecursive
          , reserved "letrec" *> pure Recursive
          ]
    in  Let <$> isRec 
            <*> sepBy1 localDefinition (reserved "and")
            <*  reserved "in"
            <*> expr
  -- , Case  <$> (reserved "case" *> expr)
  --         <*  reserved "of"
  --         <*> semiSep1 alter
  , Lam <$> (reserved "fun" *> many1 identifier)
        <*  arrow
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
          , [infixBinOp "&&" AssocRight]
          , [infixBinOp "||" AssocRight]
          ]
          (foldl1 Ap <$> many1 aexpr)
  ]
aexpr = msum
  [ Var <$> identifier
  , Num <$> natural
  , reserved "Pack" *> braces (Pack <$> nat <* comma <*> nat)
  , parens expr
  ]
  where
    nat = fromInteger <$> natural
