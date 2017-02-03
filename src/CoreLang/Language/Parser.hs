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


parseExpr :: MonadError String m => String -> String -> m (Expr SourcePos)
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

patn :: Bool -> Parser (Patn SourcePos)
patn needParens =
  case needParens of
    False -> MkPatn <$> getPosition <*> ident <*> optionMaybe asType
    True  -> 
      (MkPatn <$> getPosition <*> ident <*> pure Nothing)
      <|> parens (MkPatn <$> getPosition <*> ident <*> (Just <$> asType))
  <?> "declaration"

defn :: Parser (Defn SourcePos)
defn = 
  MkDefn <$> patn False <*> (equals *> expr)
  <?> "definition"

expr, aexpr1, aexpr :: Parser (Expr SourcePos)
expr =
  choice
    [ (Let    <$> getPosition <* reserved "let" 
       <|>
       LetRec <$> getPosition <* reserved "letrec")
       <*> sepBy1 defn (reserved "and")
       <*> (reserved "in" *> expr)
    , Lam <$> getPosition 
          <*> (reserved "fun" *> many1 (patn True))
          <*> (arrow *> expr)
    , If  <$> getPosition 
          <*> (reserved "if"   *> expr) 
          <*> (reserved "then" *> expr)
          <*> (reserved "else" *> expr)
    , let infixBinOp op = Infix $ do
            pos <- getPosition
            reservedOp op
            return (Ap pos . Ap pos (Var pos (MkIdent op)))
          partialAp = do
            pos <- getPosition
            foldl1 (Ap pos) <$> many1 aexpr
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
            partialAp
    ]
  <?> "expression"
aexpr1 = choice
  [ Var <$> getPosition <*> ident
  , Num <$> getPosition <*> nat
  , reserved "Pack" *> braces (Pack <$> getPosition <*> nat <*> (comma *> nat))
  , parens expr
  , Rec <$> getPosition <*> braces (commaSep defn)
  ]
aexpr = do
  pos <- getPosition
  foldl (Sel pos) <$> aexpr1 <*> many (reservedOp "." *> ident)
