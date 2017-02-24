module Pukeko.Language.Parser
  ( parseExpr
  , parseType
  )
  where

import Control.Monad.Except
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token    as Token

import Pukeko.Language.Operator (Spec (..))
import Pukeko.Language.Syntax
import Pukeko.Language.Type (Type, var, (~>), app, record)

import qualified Pukeko.Language.Operator as Operator

parseExpr :: MonadError String m => String -> String -> m (Expr SourcePos)
parseExpr file code =
  case parse (whiteSpace *> expr <* eof) file code of
    Left error  -> throwError (show error)
    Right expr -> return expr

parseType :: MonadError String m => String -> m Type
parseType code =
  case parse (type_ <* eof) "<input>" code of
    Left error -> throwError (show error)
    Right expr -> return expr


type Parser = Parsec String ()

pukekoDef :: LanguageDef st
pukekoDef = haskellStyle
  { Token.reservedNames =
      [ "fun"
      , "let", "letrec", "and", "in"
      , "if", "then", "else"
      ]
  , Token.opStart  = Token.opLetter pukekoDef
  , Token.opLetter = Token.opLetter haskellStyle <|> char ';'
  , Token.reservedOpNames = ["=", "->", ":", "."] ++ Operator.syms
  }

Token.TokenParser
  { Token.identifier
  , Token.reserved
  , Token.reservedOp
  , Token.natural
  , Token.parens
  , Token.braces
  , Token.commaSep
  , Token.whiteSpace
  } =
  Token.makeTokenParser pukekoDef

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
  if needParens then
    MkPatn <$> getPosition <*> ident <*> pure Nothing
    <|>
    parens (MkPatn <$> getPosition <*> ident <*> (Just <$> asType))
  else
    MkPatn <$> getPosition <*> ident <*> optionMaybe asType
  <?> "declaration"

defnVal :: Parser (Defn SourcePos)
defnVal = MkDefn <$> patn False <*> (equals *> expr)

defnFun :: Parser (Defn SourcePos)
defnFun =
  MkDefn <$> (MkPatn <$> getPosition
                     <*> ident
                     <*> pure Nothing)
         <*> (Lam <$> getPosition
                  <*> many1 (patn True)
                  <*> (equals *> expr))

defn :: Parser (Defn SourcePos)
defn = try defnVal <|> defnFun <?> "definition"

expr, aexpr :: Parser (Expr SourcePos)
expr =
  choice
    [ Let <$> getPosition
          <*> (reserved "let" *> pure False <|> reserved "letrec" *> pure True )
          <*> sepBy1 defn (reserved "and")
          <*> (reserved "in" *> expr)
    , Lam <$> getPosition
          <*> (reserved "fun" *> many1 (patn True))
          <*> (arrow *> expr)
    , If  <$> getPosition
          <*> (reserved "if"   *> expr)
          <*> (reserved "then" *> expr)
          <*> (reserved "else" *> expr)
    , let partialAp = do
            pos <- getPosition
            foldl1 (Ap pos) <$> many1 aexpr
      in  buildExpressionParser operatorTable partialAp
    ]
  <?> "expression"
aexpr = choice
  [ Var <$> getPosition <*> ident
  , Num <$> getPosition <*> nat
  , parens expr
  ]

operatorTable = map (map f) (reverse Operator.table)
  where
    f MkSpec { _sym, _name, _assoc } =
      Infix (ApOp <$> getPosition <*> (reservedOp _sym *> pure _name)) _assoc
