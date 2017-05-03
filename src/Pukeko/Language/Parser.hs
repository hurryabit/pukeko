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
import Pukeko.Language.Ident (Con)
import Pukeko.Language.Operator (Spec (..))
import Pukeko.Language.Syntax
import Pukeko.Language.Type (Type, Closed, var, (~>), app)

import qualified Pukeko.Language.Ident    as Ident
import qualified Pukeko.Language.Operator as Operator

parseModule :: MonadError String m => String -> String -> m (Module Con SourcePos)
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
      , "external"
      ]
  , Token.opStart  = oneOf Operator.letters
  , Token.opLetter = oneOf Operator.letters
  , Token.reservedOpNames = ["=", "->", ":", ".", "|"]
  }

pukeko@Token.TokenParser
  { Token.reserved
  , Token.reservedOp
  , Token.operator
  , Token.natural
  , Token.identifier
  , Token.parens
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

variable, alphaVariable :: Parser Ident.Var
alphaVariable = Ident.variable <$> (lookAhead lower *> identifier)
variable = alphaVariable <|> Ident.operator <$> try (parens operator)

constructor :: Parser Ident.Con
constructor = Ident.constructor <$> (lookAhead upper *> Token.identifier pukeko)

type_, atype :: Parser (Type Con Closed)
type_ =
  buildExpressionParser
    [ [ Infix (arrow *> pure (~>)) AssocRight ] ]
    ( app <$> constructor <*> many atype
      <|> atype
    )
  <?> "type"
atype = choice
  [ var <$> alphaVariable
  , app <$> constructor <*> pure []
  , parens type_
  ]

asType :: Parser (Type Con Closed)
asType = reservedOp ":" *> type_

module_ :: Parser (Module Con SourcePos)
module_ = many1 $ choice
  [ let_ Def <* optional (reservedOp ";;")
  , Asm <$> getPosition
        <*> (reserved "external" *> variable)
        <*> (equals *> Token.stringLiteral pukeko)
  , Val <$> getPosition
        <*> (reserved "val" *> variable)
        <*> asType
  , Type <$> getPosition
         <*> (reserved "type" *> sepBy1 adt (reserved "and"))
  ]

typed :: Bool -> (SourcePos -> a -> Maybe (Type Con Closed) -> b) -> Parser a -> Parser b
typed needParens constr ident =
  if needParens then
    constr <$> getPosition <*> ident <*> pure Nothing
    <|>
    parens (constr <$> getPosition <*> ident <*> (Just <$> asType))
  else
    constr <$> getPosition <*> ident <*> optionMaybe asType
  <?> "declaration"

bind :: Bool -> Parser (Bind Con SourcePos)
bind needParens = typed needParens MkBind $ variable

bind0 :: Bool -> Parser (Bind0 Con SourcePos)
bind0 needParens =
  typed needParens MkBind (Just <$> variable <|> symbol "_" *> pure Nothing)

defnValLhs :: Parser (Expr Con SourcePos -> Defn Con SourcePos)
defnValLhs = MkDefn <$> bind False

defnFunLhs :: Parser (Expr Con SourcePos -> Defn Con SourcePos)
defnFunLhs =
  (.) <$> (MkDefn <$> (MkBind <$> getPosition <*> variable <*> pure Nothing))
      <*> (Lam <$> getPosition <*> many1 (bind0 True))

-- TODO: Improve this code.
defn :: Parser (Defn Con SourcePos)
defn = (try defnFunLhs <|> defnValLhs) <*> (equals *> expr) <?> "definition"

altn :: Parser (Altn Con SourcePos)
altn =
  MkAltn <$> getPosition
         <*> (bar *> constructor)
         <*> many (bind0 True)
         <*> (arrow *> expr)

let_ :: (SourcePos -> Bool -> [Defn Con SourcePos] -> a) -> Parser a
let_ f =
  f <$> getPosition
    <*> (reserved "let" *> (reserved "rec" *> pure True <|> pure False))
    <*> sepBy1 defn (reserved "and")

expr, aexpr :: Parser (Expr Con SourcePos)
expr =
  buildExpressionParser operatorTable (choice
  [ mkAp <$> getPosition <*> aexpr <*> many aexpr
  , let_ Let <*> (reserved "in" *> expr)
  , Lam <$> getPosition
        <*> (reserved "fun" *> many1 (bind0 True))
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
  [ Var <$> getPosition <*> variable
  , Con <$> getPosition <*> constructor
  , Num <$> getPosition <*> nat
  , parens expr
  ]

operatorTable = map (map f) (reverse Operator.table)
  where
    f MkSpec { _sym, _assoc } = Infix (mkApOp _sym <$> (getPosition <* reservedOp _sym)) _assoc

adt :: Parser ADT
adt = mkADT <$> constructor
            <*> many variable
            <*> option [] (reservedOp "=" *> many1 adtConstructor)

adtConstructor :: Parser Constructor
adtConstructor = mkConstructor <$> (reservedOp "|" *> constructor) <*> many atype
