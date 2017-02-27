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
import Pukeko.Language.Type (Type, Closed, var, (~>), app)

import qualified Pukeko.Language.Operator as Operator

parseExpr :: MonadError String m => String -> String -> m (Expr SourcePos)
parseExpr file code =
  case parse (whiteSpace *> expr <* eof) file code of
    Left error  -> throwError (show error)
    Right expr -> return expr

parseType :: MonadError String m => String -> m (Type Closed)
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
      , "match", "with"
      ]
  , Token.opStart  = Token.opLetter pukekoDef
  , Token.opLetter = Token.opLetter haskellStyle <|> char ';'
  , Token.reservedOpNames = ["=", "->", ":", ".", "|"] ++ Operator.syms
  }

pukeko@Token.TokenParser
  { Token.reserved
  , Token.reservedOp
  , Token.natural
  , Token.parens
  -- , Token.braces
  -- , Token.commaSep
  , Token.whiteSpace
  } =
  Token.makeTokenParser pukekoDef

nat :: Parser Int
nat = fromInteger <$> natural

equals, arrow, bar :: Parser ()
equals  = reservedOp "="
arrow   = reservedOp "->"
bar     = reservedOp "|"

identifier, variable, constructor  :: Parser Ident
identifier  = MkIdent <$> Token.identifier pukeko
variable    = lookAhead lower *> identifier
constructor = lookAhead upper *> identifier

type_, atype :: Parser (Type Closed)
type_ =
  buildExpressionParser
    [ [ Infix (arrow *> pure (~>)) AssocRight ] ]
    ( app <$> constructor <*> many atype
      <|> atype
    )
  <?> "type"
atype = choice
  [ var <$> variable
  , app <$> constructor <*> pure []
  , parens type_
  ]

asType :: Parser (Type Closed)
asType = reservedOp ":" *> type_

patn :: Bool -> Parser (Patn SourcePos)
patn needParens =
  if needParens then
    MkPatn <$> getPosition <*> variable <*> pure Nothing
    <|>
    parens (MkPatn <$> getPosition <*> variable <*> (Just <$> asType))
  else
    MkPatn <$> getPosition <*> variable<*> optionMaybe asType
  <?> "declaration"

defnVal :: Parser (Defn SourcePos)
defnVal = MkDefn <$> patn False <*> (equals *> expr)

defnFun :: Parser (Defn SourcePos)
defnFun =
  MkDefn <$> (MkPatn <$> getPosition
                     <*> variable
                     <*> pure Nothing)
         <*> (Lam <$> getPosition
                  <*> many1 (patn True)
                  <*> (equals *> expr))

defn :: Parser (Defn SourcePos)
defn = try defnVal <|> defnFun <?> "definition"

altn :: Parser (Altn SourcePos)
altn =
  MkAltn <$> getPosition
         <*> (bar *> constructor)
         <*> many (patn True)
         <*> (arrow *> expr)

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
    , Match <$> getPosition
            <*> (reserved "match" *> expr)
            <*> (reserved "with"  *> many1 altn)
    , buildExpressionParser operatorTable $
        mkAp <$> getPosition <*> aexpr <*> many aexpr
    ]
  <?> "expression"
aexpr = choice
  [ Var <$> getPosition <*> identifier
  , Num <$> getPosition <*> nat
  , parens expr
  ]

operatorTable = map (map f) (reverse Operator.table)
  where
    f MkSpec { _sym, _name, _assoc } =
      Infix (ApOp <$> getPosition <*> (reservedOp _sym *> pure _name)) _assoc
