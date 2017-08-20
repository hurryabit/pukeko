module Pukeko.Language.Parser
  ( parseModule
  )
  where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import Pukeko.Error
import Pukeko.Language.Operator (Spec (..))
import Pukeko.Language.Syntax
import Pukeko.Language.Type (ADT, Constructor, mkADT, mkConstructor, Type, Closed, var, (~>), app)
import qualified Pukeko.Language.Ident    as Ident
import qualified Pukeko.Language.Operator as Operator

parseModule :: MonadError String m => String -> String -> m (Module StageLP SourcePos)
parseModule file code =
  either (throwError . show) return $ parse (whiteSpace *> module_ <* eof) file code

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

evar :: Parser Ident.EVar
evar = Ident.evar <$> (lookAhead lower *> identifier)
  <|> Ident.op <$> try (parens operator)

tvar :: Parser Ident.TVar
tvar = Ident.tvar <$> (lookAhead lower *> identifier)

constructor :: Parser Ident.Con
constructor = Ident.constructor <$> (lookAhead upper *> Token.identifier pukeko)

type_, atype :: Parser (Type Ident.Con Closed)
type_ =
  buildExpressionParser
    [ [ Infix (arrow *> pure (~>)) AssocRight ] ]
    ( app <$> constructor <*> many atype
      <|> atype
    )
  <?> "type"
atype = choice
  [ var <$> tvar
  , app <$> constructor <*> pure []
  , parens type_
  ]

asType :: Parser (Type Ident.Con Closed)
asType = reservedOp ":" *> type_

module_ :: Parser (Module StageLP SourcePos)
module_ = many1 $ choice
  [ let_ Def
  , Asm <$> getPosition
        <*> (reserved "external" *> evar)
        <*> (equals *> Token.stringLiteral pukeko)
  , Val <$> getPosition
        <*> (reserved "val" *> evar)
        <*> asType
  , Type <$> getPosition
         <*> (reserved "type" *> sepBy1 adt (reserved "and"))
  ]

bind0 :: Parser (Bind0 StageLP SourcePos)
bind0  =
  MkBind <$> getPosition <*> (Just <$> evar <|> symbol "_" *> pure Nothing)

defnValLhs :: Parser (Expr StageLP SourcePos -> Defn StageLP SourcePos)
defnValLhs = MkDefn <$> getPosition <*> evar

defnFunLhs :: Parser (Expr StageLP SourcePos -> Defn StageLP SourcePos)
defnFunLhs =
  (.) <$> (MkDefn <$> getPosition <*> evar)
      <*> (Lam <$> getPosition <*> many1 bind0)

-- TODO: Improve this code.
defn :: Parser (Defn StageLP SourcePos)
defn = (try defnFunLhs <|> defnValLhs) <*> (equals *> expr) <?> "definition"

altn :: Parser (Altn StageLP SourcePos)
altn =
  MkAltn <$> getPosition
         <*> (bar *> constructor)
         <*> many bind0
         <*> (arrow *> expr)

let_ :: (SourcePos -> Bool -> [Defn StageLP SourcePos] -> a) -> Parser a
let_ f =
  f <$> getPosition
    <*> (reserved "let" *> (reserved "rec" *> pure True <|> pure False))
    <*> sepBy1 defn (reserved "and")

expr, aexpr :: Parser (Expr StageLP SourcePos)
expr =
  buildExpressionParser operatorTable (choice
  [ mkAp <$> getPosition <*> aexpr <*> many aexpr
  , let_ Let <*> (reserved "in" *> expr)
  , Lam <$> getPosition
        <*> (reserved "fun" *> many1 bind0)
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
  [ Var <$> getPosition <*> evar
  , Con <$> getPosition <*> constructor
  , Num <$> getPosition <*> nat
  , parens expr
  ]

operatorTable = map (map f) (reverse Operator.table)
  where
    f MkSpec { _sym, _assoc } = Infix (mkApOp _sym <$> (getPosition <* reservedOp _sym)) _assoc

adt :: Parser (ADT Ident.Con)
adt = mkADT' <$> constructor
             <*> many tvar
             <*> option [] (reservedOp "=" *> many1 adtConstructor)
  where mkADT' con = mkADT con con

adtConstructor :: Parser (Constructor Ident.Con)
adtConstructor = mkConstructor <$> (reservedOp "|" *> constructor) <*> many atype
