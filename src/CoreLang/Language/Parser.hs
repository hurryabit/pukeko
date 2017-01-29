module CoreLang.Language.Parser 
  ( parseExpr
  , parseType
  )
  where

import Control.Monad (msum)
import Control.Monad.Except
import Text.Parsec -- (Parsec, parse, choice, eof, lookAhead, many, many1, sepBy1, try)
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token    as Token

import CoreLang.Language.Syntax
import CoreLang.Language.Type (Type (..), var, cons, (~>))


parseExpr :: MonadError String m => String -> String -> m Expr
parseExpr file code = 
  case parse (expr <* eof) file code of
    Left error  -> throwError (show error)
    Right expr -> return expr

parseType :: MonadError String m => String -> m Type
parseType code = 
  case parse (type_ <* eof) "<input>" code of
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
  , Token.colon
  } =
  Token.makeTokenParser coreLangDef

equals = symbol "="

arrow = symbol "->"

type Parser = Parsec String ()

type_, atype :: Parser Type
type_ =
  buildExpressionParser
    [ [ Infix (reservedOp "->" *> pure (~>)) AssocRight ] ]    
    (typeCons <|> atype)
  <?> "type"
atype = choice
  [ typeVar
  , typeCons0
  , parens type_
  ]
typeVar   = try (lookAhead upper *> (var  <$> identifier))
typeCons  = try (lookAhead lower *> (cons <$> identifier <*> many atype))
typeCons0 = try (lookAhead lower *> (cons <$> identifier <*> pure []))

declaration :: Bool -> Parser Declaration
declaration needParens =
  case needParens of
    False -> (,) <$> identifier <*> optionMaybe (colon *> type_)
    True  -> 
      ((,) <$> identifier <*> pure Nothing)
      <|> parens ((,) <$> identifier <*> (Just <$> type_))
  <?> "declaration"

definition :: Parser Definition
definition = 
  (,) <$> declaration False <* equals <*> expr 
  <?> "definition"

expr, aexpr :: Parser Expr
expr =
  choice
    [ let isRec = msum
            [ reserved "let"    *> pure NonRecursive
            , reserved "letrec" *> pure Recursive
            ]
      in  Let <$> isRec 
              <*> sepBy1 definition (reserved "and")
              <*  reserved "in"
              <*> expr
    , Lam <$> (reserved "fun" *> many1 (declaration True))
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
  <?> "expression"
aexpr = choice
  [ Var <$> identifier
  , Num <$> natural
  , reserved "Pack" *> braces (Pack <$> nat <* comma <*> nat)
  , parens expr
  ]
  where
    nat = fromInteger <$> natural
