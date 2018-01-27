{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | Implementation of the parser.
module Pukeko.FrontEnd.Parser
  ( Module
  , Package
  , parseInput
  , parseModule
  , parsePackage
  , extend
  )
  where

import Pukeko.Prelude hiding (lctd)

import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as Set
import           System.FilePath            as Sys
import           Text.Megaparsec
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

import           Pukeko.AST.Operator   (Spec (..))
import           Pukeko.AST.Surface
import qualified Pukeko.AST.Identifier as Id
import qualified Pukeko.AST.Operator   as Op
import           Pukeko.FrontEnd.Parser.Build (build)

parseInput :: (Member (Error Doc) effs) => FilePath -> String -> Eff effs Module
parseInput file =
  either (throwError . text . parseErrorPretty) pure . parse (module_ file <* eof) file

parseModule ::
  (Member (Error Doc) effs, LastMember IO effs) => FilePath -> Eff effs Module
parseModule file = do
  sendM (putStr (file ++ " "))
  code <- sendM (readFile file)
  parseInput file code

parsePackage ::
  (Member (Error Doc) effs, LastMember IO effs) => FilePath -> Eff effs Package
parsePackage file = build parseModule file <* sendM (putStrLn "")

type Parser = Parsec Void String

space :: Parser ()
space = L.space space1 (L.skipLineComment "--") empty

indented :: Parser a -> Parser a
indented p = L.indentGuard space GT pos1 *> p

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

comma :: Parser ()
comma = void (symbol ",")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- TODO: Ideally we want a trie rather than a tree.
reservedIdents :: Set String
reservedIdents = Set.fromList
      [ "fun"
      , "let", "rec", "and", "in"
      , "if", "then", "else"
      , "match", "with"
      , "type", "class", "instance"
      , "external"
      , "import"
      ]

lIdentStart :: Parser Char
lIdentStart = lowerChar

uIdentStart :: Parser Char
uIdentStart = upperChar

identLetter :: Parser Char
identLetter = alphaNumChar <|> satisfy (\c -> c == '_' || c == '\'')

reservedOperators :: Set String
reservedOperators = Set.fromList ["->", "=>", "=", ":", "|"]

opLetter :: Parser Char
opLetter = oneOf (Set.fromList "+-*/%=<>|&!.:;")

reserved :: String -> Parser ()
reserved kw = void (lexeme (string kw <* notFollowedBy identLetter))

reservedOp :: String -> Parser ()
reservedOp op = void (lexeme (string op <* notFollowedBy opLetter))

lIdent :: Parser String
lIdent = lexeme $ try $ do
  x <- (:) <$> lIdentStart <*> many identLetter
  when (x `Set.member` reservedIdents) $
    unexpected (Label (NE.fromList ("reserved " ++ x)))
  pure x

uIdent :: Parser String
uIdent = lexeme $ (:) <$> uIdentStart <*> many identLetter

operator :: Parser String
operator = lexeme $ try $ do
  op <- some opLetter
  when (op `Set.member` reservedOperators) $
    unexpected (Label (NE.fromList ("reserved op " ++ op)))
  pure op

decimal :: Parser Int
decimal = lexeme L.decimal

lctd :: Parser a -> Parser (Lctd a)
lctd p = Lctd <$> getPosition <*> p

equals, arrow, darrow, colon, bar :: Parser ()
equals  = reservedOp "="
arrow   = reservedOp "->"
darrow  = reservedOp "=>"
colon   = reservedOp ":"
bar     = reservedOp "|"

evar :: Parser Id.EVar
evar = Id.evar <$> lIdent
  <|> Id.op <$> try (parens operator)
  <?> "expression variable"

tvar :: Parser Id.TVar
tvar = Id.tvar <$> lIdent <?> "type variable"

tcon :: Parser Id.TCon
tcon = Id.tcon <$> uIdent <?> "type constructor"

dcon :: Parser Id.DCon
dcon = Id.dcon <$> uIdent <?> "data constructor"

clss :: Parser Id.Clss
clss = Id.clss <$> uIdent <?> "class name"

type_, atype :: Parser Type
type_ =
  makeExprParser
    (mkTApp <$> atype <*> many atype)
    [ [ InfixR (arrow *> pure mkTFun) ] ]
  <?> "type"
atype = choice
  [ TVar <$> try (indented tvar)
  , TCon <$> tcon
  , indented (parens type_)
  ]

typeCstr :: Parser TypeCstr
typeCstr = MkTypeCstr
  <$> option [] (try (parens (sepBy1 ((,) <$> clss <*> tvar) comma) <* darrow))

typeScheme :: Parser TypeScheme
typeScheme = MkTypeScheme <$> typeCstr <*> type_

-- <patn>  ::= <apatn> | <con> <apatn>*
-- <apatn> ::= '_' | <evar> | <con> | '(' <patn> ')'
patn, apatn :: Parser Patn
patn  = PCon <$> dcon <*> many apatn <|>
        apatn
apatn = symbol "_" *> pure PWld   <|>
        PVar <$> evar             <|>
        PCon <$> dcon <*> pure [] <|>
        parens patn

defnValLhs :: Parser (Expr Id.EVar -> Defn Id.EVar)
defnValLhs = MkDefn <$> evar

defnFunLhs :: Parser (Expr Id.EVar -> Defn Id.EVar)
defnFunLhs =
  (.) <$> (MkDefn <$> evar)
      <*> (ELam <$> NE.some evar)

-- TODO: Improve this code.
defn :: Parser (Defn Id.EVar)
defn = (try defnFunLhs <|> defnValLhs) <*> (equals *> lexpr) <?> "definition"

altn :: Parser (Altn Id.EVar)
altn = MkAltn <$> (bar *> patn) <*> (arrow *> expr)

let_ ::
  (NonEmpty (Lctd (Defn Id.EVar)) -> a) ->
  (NonEmpty (Lctd (Defn Id.EVar)) -> a) ->
  Parser a
let_ mkLet mkRec =
  (reserved "let" *> (reserved "rec" *> pure mkRec <|> pure mkLet))
  <*> NE.sepBy1 (lctd defn) (reserved "and")

expr, aexpr, lexpr :: Parser (Expr Id.EVar)
expr =
  (flip makeExprParser operatorTable . choice)
  [ mkApp <$> aexpr <*> many aexpr
  , mkIf  <$> (reserved "if"   *> expr)
          <*> (reserved "then" *> expr)
          <*> (reserved "else" *> expr)
  , EMat
    <$> (reserved "match" *> expr)
    <*> (reserved "with"  *> some altn)
  , ELam
    <$> (reserved "fun" *> NE.some evar)
    <*> (arrow *> expr)
  , let_ ELet ERec <*> (reserved "in" *> expr)
  ]
  <?> "expression"
aexpr = choice
  [ EVar <$> try (indented evar)
  , ECon <$> dcon
  , ENum <$> decimal
  , indented (parens expr)
  ]
lexpr = ELoc <$> lctd expr

operatorTable = map (map f) (reverse Op.table)
  where
    f MkSpec{_sym, _assoc} = g _assoc (try (reservedOp _sym *> pure (mkAppOp _sym)))
    g = \case
      Op.AssocLeft  -> InfixL
      Op.AssocRight -> InfixR
      Op.AssocNone  -> InfixN

tconDecl :: Parser TConDecl
tconDecl = MkTConDecl
  <$> tcon
  <*> many (indented tvar)
  <*> option [] (equals *> some (lctd dconDecl))

dconDecl :: Parser DConDecl
dconDecl = MkDConDecl <$> (bar *> dcon) <*> many atype

signDecl :: Parser SignDecl
signDecl = MkSignDecl <$> try (evar <* colon) <*> typeScheme

clssDecl :: Parser ClssDecl
clssDecl = MkClssDecl <$> clss <*> tvar <*> (colon *> record signDecl)

instDecl :: Parser InstDecl
instDecl = do
  c       <- clss
  (t, vs) <- (,) <$> tcon <*> pure [] <|> parens ((,) <$> tcon <*> many tvar)
  q       <- equals *> typeCstr
  ds      <- record (lctd defn)
  pure (MkInstDecl c t vs q ds)

primDecl :: Parser PrimDecl
primDecl = MkPrimDecl
  <$> evar
  <*> (equals *> char '\"' *> some lowerChar <* symbol "\"")

import_ :: Parser FilePath
import_ = do
  reserved "import"
  comps <- sepBy1 (some (lowerChar <|> digitChar <|> char '_')) (char '/')
  void eol
  pure (Sys.joinPath comps Sys.<.> "pu")

module_ :: FilePath -> Parser Module
module_ file = do
  imps <- lexeme (many import_)
  decls <- many $ lctd $ choice
    [ DType <$> (reserved "type"     *> NE.sepBy1 (lctd tconDecl) (reserved "and"))
    , DClss <$> (reserved "class"    *> clssDecl)
    , DInst <$> (reserved "instance" *> instDecl)
    , nonIndented (DSign <$> signDecl)
    , nonIndented (DDefn <$> defn)
    , DPrim <$> (reserved "external" *> primDecl)
    ]
  pure (MkModule file imps decls)

record :: Parser a -> Parser [a]
record p = braces (sepBy p comma)
