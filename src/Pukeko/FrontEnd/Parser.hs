-- | Implementation of the parser.
module Pukeko.FrontEnd.Parser
  ( Module
  , Package
  , parseInput
  , parseModule
  , parsePackage
  , extend
  , parseTest
  )
  where

import Pukeko.Prelude hiding (lctd)

import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Control.Monad.RWS          as RWS
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as Set
import           System.FilePath            as Sys
import           Text.Megaparsec            hiding (parse, parseTest)
import qualified Text.Megaparsec            as MP
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

import           Pukeko.AST.Operator   (Spec (..))
import           Pukeko.AST.Surface
import qualified Pukeko.AST.Identifier as Id
import qualified Pukeko.AST.Operator   as Op
import           Pukeko.FrontEnd.Parser.Build (build)

parseInput :: (Member (Error Failure) effs) => FilePath -> String -> Eff effs Module
parseInput file =
  either (throwFailure . pretty . parseErrorPretty) pure .
  parse (module_ file <* eof) file

parseModule ::
  (Member (Error Failure) effs, LastMember IO effs) => FilePath -> Eff effs Module
parseModule file = do
  sendM (putStr (file ++ " "))
  code <- sendM (readFile file)
  parseInput file code

parsePackage ::
  (Member (Error Failure) effs, LastMember IO effs) => FilePath -> Eff effs Package
parsePackage file = build parseModule file <* sendM (putStrLn "")

type Parser = RWS.RWST (Pos, Ordering) () SourcePos (Parsec Void String)

parse :: Parser a -> FilePath -> String -> Either (ParseError Char Void) a
parse p file = MP.parse (fst <$> RWS.evalRWST p (pos1, EQ) (initialPos file)) file

parseTest :: (Show a) => Parser a -> String -> IO ()
parseTest p = either (putStrLn . parseErrorPretty) print . parse p ""

space :: Parser ()
space = do
  L.space space1 (L.skipLineComment "--") empty
  refPos <- RWS.get
  curPos <- getPosition
  when (sourceLine refPos < sourceLine curPos) (RWS.put curPos)

indentGuard :: Parser Pos
indentGuard = do
  (refCol, refOrd) <- RWS.ask
  L.indentGuard (pure ()) refOrd refCol

lexeme :: Parser a -> Parser a
lexeme p = indentGuard *> L.lexeme space p

block :: Pos -> Ordering -> Parser a -> Parser a
block col ord = RWS.local (const (col, ord))

indented :: Parser a -> (a -> Parser b) -> Parser b
indented p f = do
  _ <- indentGuard
  refCol <- RWS.gets sourceColumn
  x <- p
  block refCol GT (f x)

indented_ :: Parser () -> Parser a -> Parser a
indented_ p0 p1 = indented p0 (const p1)

aligned :: Parser a -> Parser a
aligned p = do
  refCol <- indentGuard
  block refCol EQ p

symbol :: String -> Parser String
symbol = lexeme . string

comma :: Parser ()
comma = void (symbol ",")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- TODO: Ideally we want a trie rather than a tree.
reservedIdents :: Set String
reservedIdents = Set.fromList
      [ "fun"
      , "let", "rec", "and", "in"
      , "if", "then", "else"
      , "match", "with"
      , "type", "class", "where", "instance"
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

prefixOperator :: Parser String
prefixOperator = lexeme $ try $ do
  op <- between (char '(') (char ')') (some opLetter)
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
  <|> Id.op <$> prefixOperator
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
  [ TVar <$> tvar
  , TCon <$> tcon
  , parens type_
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

defn :: Parser (Defn Id.EVar)
defn = indented evar $ \z -> MkDefn z <$> (mkLam <$> many evar <*> (equals *> lexpr))

exprMatch :: Parser (Expr Id.EVar)
exprMatch = do
  tokCol <- indentGuard
  fstCol <- RWS.gets sourceColumn
  reserved "match"
  if tokCol == fstCol && fstCol > pos1
    then
      EMat
      <$> block tokCol GT (expr <* reserved "with")
      <*> block tokCol EQ (some altn)
    else
      block fstCol GT $
      EMat
      <$> (expr <* reserved "with")
      <*> aligned (some altn)

altn :: Parser (Altn Id.EVar)
altn = indented_ bar (MkAltn <$> patn <*> (arrow *> expr))

let_ ::
  (NonEmpty (Lctd (Defn Id.EVar)) -> a) ->
  (NonEmpty (Lctd (Defn Id.EVar)) -> a) ->
  Parser a
let_ mkLet mkRec =
  (reserved "let" *> (reserved "rec" *> pure mkRec <|> pure mkLet))
  <*> NE.sepBy1 (lctd defn) (reserved "and")

expr, aexpr, lexpr :: Parser (Expr Id.EVar)
lexpr = ELoc <$> lctd expr
aexpr = choice
  [ EVar <$> evar
  , ECon <$> dcon
  , ENum <$> decimal
  , parens expr
  ]
expr =
  (flip makeExprParser operatorTable . choice)
  [ mkApp <$> aexpr <*> many aexpr
  , mkIf  <$> (reserved "if"   *> expr)
          <*> (reserved "then" *> expr)
          <*> (reserved "else" *> expr)
  , exprMatch
  , ELam
    <$> (reserved "fun" *> NE.some evar)
    <*> (arrow *> expr)
  , let_ ELet ERec <*> (reserved "in" *> expr)
  ]
  <?> "expression"
  where
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
  <*> many tvar
  <*> option [] (equals *> aligned (some (lctd dconDecl)))

dconDecl :: Parser DConDecl
dconDecl = indented_ bar (MkDConDecl <$> dcon <*> many atype)

signDecl :: Parser SignDecl
signDecl = indented
  (try (indented evar (\z -> colon *> pure z)))
  (\z -> MkSignDecl z <$> typeScheme)

clssDecl :: Parser ClssDecl
clssDecl =
  MkClssDecl <$> clss <*> tvar <*> (reserved "where" *> aligned (many signDecl))

instDecl :: Parser InstDecl
instDecl = do
  q       <- typeCstr
  c       <- clss
  (t, vs) <- (,) <$> tcon <*> pure [] <|> parens ((,) <$> tcon <*> many tvar)
  ds      <- reserved "where" *> aligned (many (lctd defn))
  pure (MkInstDecl c t vs q ds)

primDecl :: Parser PrimDecl
primDecl = MkPrimDecl
  <$> evar
  <*> (equals *> char '\"' *> some lowerChar <* symbol "\"")

import_ :: Parser FilePath
import_ = indented_ (reserved "import") $ do
  comps <- lexeme (sepBy1 (some (lowerChar <|> digitChar <|> char '_')) (char '/'))
  pure (Sys.joinPath comps Sys.<.> "pu")

module_ :: FilePath -> Parser Module
module_ file = do
  space
  imps <- many import_
  decls <- many $ lctd $ choice
    [ DType <$> ( (:|)
                  <$> indented_ (reserved "type") (lctd tconDecl)
                  <*> many (indented_ (reserved "and") (lctd tconDecl))
                )
    , DClss <$> indented_ (reserved "class") clssDecl
    , DInst <$> indented_ (reserved "instance") instDecl
    , DPrim <$> indented_ (reserved "external") primDecl
    , DSign <$> signDecl <?> "function declaration"
    , DDefn <$> defn <?> "function definition"
    ]
  pure (MkModule file imps decls)
