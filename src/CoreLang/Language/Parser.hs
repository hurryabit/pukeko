module CoreLang.Language.Parser 
  ( parseExpr
  )
  where

import Control.Monad (msum)
import Control.Monad.Except
import Text.Parsec as Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token

import CoreLang.Language.Syntax


parseExpr :: MonadError String m => String -> String -> m Expr
parseExpr file code = 
  case Parsec.parse (expr <* eof) file code of
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

definition :: Parser Definition
definition =
  (,) <$> identifier <* equals <*> expr

expr, aexpr :: Parser Expr
expr = msum
  [ let isRec = msum
          [ reserved "let"    *> pure NonRecursive
          , reserved "letrec" *> pure Recursive
          ]
    in  Let <$> isRec 
            <*> sepBy1 definition (reserved "and")
            <*  reserved "in"
            <*> expr
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
