module CoreLang.Language.Parser 
  ( parseExpr
  , parseType
  )
  where

import Control.Monad.Except
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token    as Token

import CoreLang.Language.Syntax
import CoreLang.Language.Type (Type, var, (~>), app, record)


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
      , ["=", "->", ":", "."]
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
  , Token.commaSep
  } =
  Token.makeTokenParser coreLangDef

nat :: Parser Int
nat = fromInteger <$> natural

equals, arrow :: Parser ()
equals  = reservedOp "="
arrow   = reservedOp "->"

ident, typeName  :: Parser Ident
ident = MkIdent <$> identifier
typeName = lookAhead lower *> ident

typeVar :: Parser Type
typeVar = var <$> (lookAhead upper *> identifier)

type_, atype :: Parser Type
type_ =
  buildExpressionParser
    [ [ Infix (arrow *> pure (~>)) AssocRight ] ]
    ( app <$> typeName <*> many atype
      <|> atype
    )
  <?> "type"
atype = choice
  [ typeVar
  , app <$> typeName <*> pure []
  , record <$> braces (commaSep ((,) <$> ident <*> asType))
  , parens type_
  ]
  
asType :: Parser Type
asType = reservedOp ":" *> type_

decl :: Bool -> Parser Decl
decl needParens =
  case needParens of
    False -> MkDecl <$> ident <*> optionMaybe asType
    True  -> 
      (MkDecl <$> ident <*> pure Nothing)
      <|> parens (MkDecl <$> ident <*> (Just <$> asType))
  <?> "declaration"

defn :: Parser Defn
defn = 
  MkDefn <$> decl False <*> (equals *> expr)
  <?> "definition"

expr, aexpr1, aexpr :: Parser Expr
expr =
  choice
    [ let let_ =
            choice
              [ reserved "let"    *> pure Let
              , reserved "letrec" *> pure LetRec
              ]
      in  let_ <*> sepBy1 defn (reserved "and")
               <*> (reserved "in" *> expr)
    , Lam <$> (reserved "fun" *> many1 (decl True))
          <*> (arrow *> expr)
    , If  <$> (reserved "if"   *> expr) 
          <*> (reserved "then" *> expr)
          <*> (reserved "else" *> expr)
    , let infixBinOp op = Infix $ reservedOp op *> pure (Ap . Ap (Var (MkIdent op)))
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
aexpr1 = choice
  [ Var <$> ident
  , Num <$> nat
  , reserved "Pack" *> braces (Pack <$> nat <*> (comma *> nat))
  , parens expr
  , Rec <$> braces (commaSep defn)
  ]
aexpr = foldl Sel <$> aexpr1 <*> many (reservedOp "." *> ident)
