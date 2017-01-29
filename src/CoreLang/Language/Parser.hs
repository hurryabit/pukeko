module CoreLang.Language.Parser 
  ( parseExpr
  , parseType
  )
  where

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


type Parser = Parsec String ()

relOpNames = ["<", "<=", "==", "!=", ">=", ">"]

coreLangDef = Language.haskellStyle
  { Token.reservedNames =
      [ "fun"
      , "let", "letrec", "and", "in"
      , "if", "then", "else"
      -- , "case", "of"
      , "Pack"
      ]
  , Token.reservedOpNames = concat
      [ ["+", "-", "*", "/"]
      , relOpNames
      , ["&&", "||"]
      , ["->", "="]
      ]
  }

Token.TokenParser
  { Token.identifier
  , Token.reserved
  , Token.reservedOp
  , Token.natural
  , Token.parens
  , Token.braces
  , Token.comma
  , Token.colon
  } =
  Token.makeTokenParser coreLangDef

equals = reservedOp "="
arrow  = reservedOp "->"

type_, atype :: Parser Type
type_ =
  buildExpressionParser
    [ [ Infix (arrow *> pure (~>)) AssocRight ] ]    
    (typeCons <|> atype)
  <?> "type"
atype = choice
  [ typeVar
  , typeCons0
  , parens type_
  ]
typeVar   = try (lookAhead upper *> (var  <$> identifier               ))
typeCons  = try (lookAhead lower *> (cons <$> identifier <*> many atype))
typeCons0 = try (lookAhead lower *> (cons <$> identifier <*> pure []   ))

declaration :: Bool -> Parser Declaration
declaration needParens =
  case needParens of
    False -> (,) <$> identifier <*> optionMaybe (colon *> type_)
    True  -> 
      ((,) <$> identifier <*> pure Nothing)
      <|> parens ((,) <$> identifier <*> (Just <$> (colon *> type_)))
  <?> "declaration"

definition :: Parser Definition
definition = 
  (,) <$> declaration False <*> (equals *> expr)
  <?> "definition"

expr, aexpr :: Parser Expr
expr =
  choice
    [ let let_ =
            choice
              [ reserved "let"    *> pure Let
              , reserved "letrec" *> pure LetRec
              ]
      in  let_ <*> sepBy1 definition (reserved "and")
               <*> (reserved "in" *> expr)
    , Lam <$> (reserved "fun" *> many1 (declaration True))
          <*> (arrow *> expr)
    , If  <$> (reserved "if"   *> expr) 
          <*> (reserved "then" *> expr)
          <*> (reserved "else" *> expr)
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
  , reserved "Pack" *> braces (Pack <$> nat <*> (comma *> nat))
  , parens expr
  ]
  where
    nat = fromInteger <$> natural
