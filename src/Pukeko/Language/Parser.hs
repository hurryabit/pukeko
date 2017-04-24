module Pukeko.Language.Parser
  ( parseModule
  )
  where

import Control.Monad.Except
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token    as Token

import Pukeko.Language.ADT (ADT, Constructor, mkADT, mkConstructor)
import Pukeko.Language.Operator (Spec (..))
import Pukeko.Language.Syntax
import Pukeko.Language.Type (Type, Closed, var, (~>), app)

import qualified Pukeko.Language.Operator as Operator

parseModule :: MonadError String m => String -> String -> m (Module SourcePos)
parseModule file code =
  case parse (whiteSpace *> module_ <* eof) file code of
    Left error  -> throwError (show error)
    Right expr -> return expr

type Parser = Parsec String ()

pukekoDef :: LanguageDef st
pukekoDef = haskellStyle
  { Token.reservedNames =
      [ "fun"
      , "val", "let", "rec", "and", "in"
      , "if", "then", "else"
      , "match", "with"
      , "type"
      , "asm"
      ]
  , Token.opStart  = Token.opLetter pukekoDef
  , Token.opLetter = Token.opLetter haskellStyle <|> char ';'
  , Token.reservedOpNames = ["=", "->", ":", ".", "|", ";;"] ++ Operator.syms
  }

pukeko@Token.TokenParser
  { Token.reserved
  , Token.reservedOp
  , Token.natural
  , Token.parens
  -- , Token.braces
  -- , Token.commaSep
  , Token.symbol
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

module_ :: Parser (Module SourcePos)
module_ = many1 $ choice
  [ let_ Def <* optional (reservedOp ";;")
  , Asm <$> getPosition
        <*> (reserved "asm" *> variable)
  , Val <$> getPosition
        <*> (reserved "val" *> variable)
        <*> asType
  , Type <$> getPosition
         <*> (reserved "type" *> sepBy1 adt (reserved "and"))
  ]

typed :: Bool -> (SourcePos -> a -> Maybe (Type Closed) -> b) -> Parser a -> Parser b
typed needParens constr ident =
  if needParens then
    constr <$> getPosition <*> ident <*> pure Nothing
    <|>
    parens (constr <$> getPosition <*> ident <*> (Just <$> asType))
  else
    constr <$> getPosition <*> ident <*> optionMaybe asType
  <?> "declaration"

bind :: Bool -> Parser (Bind SourcePos)
bind needParens =
  typed needParens MkBind $ (Just <$> variable) <|> (symbol "_" *> pure Nothing)

defnValLhs :: Parser (Expr SourcePos -> Defn SourcePos)
defnValLhs = typed False MkDefn variable

defnFunLhs :: Parser (Expr SourcePos -> Defn SourcePos)
defnFunLhs =
  (.) <$> (MkDefn <$> getPosition
                  <*> variable
                  <*> pure Nothing)
      <*> (Lam <$> getPosition
               <*> many1 (bind True))

-- TODO: Improve this code.
defn :: Parser (Defn SourcePos)
defn = (try defnFunLhs <|> defnValLhs) <*> (equals *> expr) <?> "definition"

altn :: Parser (Altn SourcePos)
altn =
  MkAltn <$> getPosition
         <*> (bar *> constructor)
         <*> many (bind True)
         <*> (arrow *> expr)

let_ :: (SourcePos -> Bool -> [Defn SourcePos] -> a) -> Parser a
let_ f =
  f <$> getPosition
    <*> (reserved "let" *> (reserved "rec" *> pure True <|> pure False))
    <*> sepBy1 defn (reserved "and")

expr, aexpr :: Parser (Expr SourcePos)
expr =
  buildExpressionParser operatorTable (choice
  [ mkAp <$> getPosition <*> aexpr <*> many aexpr
  , let_ Let <*> (reserved "in" *> expr)
  , Lam <$> getPosition
        <*> (reserved "fun" *> many1 (bind True))
        <*> (arrow *> expr)
  , If  <$> getPosition
        <*> (reserved "if"   *> expr)
        <*> (reserved "then" *> expr)
        <*> (reserved "else" *> expr)
  , Match <$> getPosition
          <*> (reserved "match" *> expr)
          <*> (reserved "with"  *> many1 altn)
  ])
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

adt :: Parser ADT
adt = mkADT <$> constructor
            <*> many variable
            <*> option [] (reservedOp "=" *> many1 adtConstructor)

adtConstructor :: Parser Constructor
adtConstructor = mkConstructor <$> (reservedOp "|" *> constructor) <*> many atype
