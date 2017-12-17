-- | Implementation of the parser.
module Pukeko.Language.Parser
  ( Module
  , parseModule
  )
  where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import           Pukeko.Error
import           Pukeko.Language.Operator   (Spec (..))
import           Pukeko.Language.AST.Std    hiding (StdExpr (..), StdAltn (..))
import           Pukeko.Language.Parser.AST
import qualified Pukeko.Language.Type       as Ty
import qualified Pukeko.Language.Ident      as Id
import qualified Pukeko.Language.Operator   as Op

parseModule :: MonadError String m => String -> String -> m Module
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
  , Token.opStart  = oneOf Op.letters
  , Token.opLetter = oneOf Op.letters
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

evar :: Parser Id.EVar
evar = Id.evar <$> (lookAhead lower *> identifier)
  <|> Id.op <$> try (parens operator)

tvar :: Parser Id.TVar
tvar = Id.tvar <$> (lookAhead lower *> identifier)

constructor :: Parser Id.Con
constructor = Id.constructor <$> (lookAhead upper *> Token.identifier pukeko)

type_, atype :: Parser (Ty.Type TypeCon Ty.Closed)
type_ =
  buildExpressionParser
    [ [ Infix (arrow *> pure (Ty.~>)) AssocRight ] ]
    ( Ty.app <$> constructor <*> many atype
      <|> atype
    )
  <?> "type"
atype = choice
  [ Ty.var <$> tvar
  , Ty.app <$> constructor <*> pure []
  , parens type_
  ]

asType :: Parser (Ty.Type TypeCon Ty.Closed)
asType = reservedOp ":" *> type_

module_ :: Parser Module
module_ = many1 $ choice
  [ let_ TopLet TopRec
  , Asm <$> getPosition
        <*> (reserved "external" *> evar)
        <*> (equals *> Token.stringLiteral pukeko)
  , Val <$> getPosition
        <*> (reserved "val" *> evar)
        <*> asType
  , TypDef <$> getPosition
           <*> (reserved "type" *> sepBy1 adt (reserved "and"))
  ]

bind :: Parser Bind
bind = Wild <$> getPosition <*  symbol "_" <|>
       Name <$> getPosition <*> evar


-- <patn>  ::= <apatn> | <con> <apatn>*
-- <apatn> ::= '_' | <evar> | <con> | '(' <patn> ')'
patn, apatn :: Parser Patn
patn  = Dest <$> getPosition <*> constructor <*> many apatn <|>
        apatn
apatn = Bind <$> bind <|>
        Dest <$> getPosition <*> constructor <*> pure [] <|>
        parens patn

defnValLhs :: Parser (Expr Id.EVar -> Defn Id.EVar)
defnValLhs = MkDefn <$> getPosition <*> evar

defnFunLhs :: Parser (Expr Id.EVar -> Defn Id.EVar)
defnFunLhs =
  (.) <$> (MkDefn <$> getPosition <*> evar)
      <*> (Lam <$> getPosition <*> many1 bind)

-- TODO: Improve this code.
defn :: Parser (Defn Id.EVar)
defn = (try defnFunLhs <|> defnValLhs) <*> (equals *> expr) <?> "definition"

altn :: Parser (Altn Id.EVar)
altn =
  MkAltn <$> getPosition
         <*> (bar *> patn)
         <*> (arrow *> expr)

let_ :: (Pos -> [Defn Id.EVar] -> a) -> (Pos -> [Defn Id.EVar] -> a) -> Parser a
let_ mkLet mkRec =
  f <$> getPosition
    <*> (reserved "let" *> (reserved "rec" *> pure mkRec <|> pure mkLet))
    <*> sepBy1 defn (reserved "and")
  where
    f w mk = mk w

expr, aexpr :: Parser (Expr Id.EVar)
expr =
  (buildExpressionParser operatorTable . choice)
  [ mkApp <$> getPosition <*> aexpr <*> many aexpr
  , mkIf  <$> getPosition
          <*> (reserved "if"   *> expr)
          <*> getPosition
          <*> (reserved "then" *> expr)
          <*> getPosition
          <*> (reserved "else" *> expr)
  , Mat <$> getPosition
        <*> (reserved "match" *> expr)
        <*> (reserved "with"  *> many1 altn)
  , Lam <$> getPosition
        <*> (reserved "fun" *> many1 bind)
        <*> (arrow *> expr)
  , let_ Let Rec <*> (reserved "in" *> expr)
  ]
  <?> "expression"
aexpr = choice
  [ Var <$> getPosition <*> evar
  , Con <$> getPosition <*> constructor
  , Num <$> getPosition <*> nat
  , parens expr
  ]

operatorTable = map (map f) (reverse Op.table)
  where
    f MkSpec { _sym, _assoc } = Infix (mkAppOp _sym <$> (getPosition <* reservedOp _sym)) _assoc

adt :: Parser (Ty.ADT Id.Con)
adt = mkADT' <$> constructor
             <*> many tvar
             <*> option [] (reservedOp "=" *> many1 adtConstructor)
  where mkADT' con = Ty.mkADT con con

adtConstructor :: Parser (Ty.Constructor Id.Con)
adtConstructor = Ty.mkConstructor <$> (reservedOp "|" *> constructor) <*> many atype
