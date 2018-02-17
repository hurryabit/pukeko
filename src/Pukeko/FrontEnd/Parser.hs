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

import Pukeko.Prelude hiding (lctd, many, some)

import qualified Control.Applicative.Combinators.NonEmpty as NE
import qualified Control.Monad.RWS          as RWS
import qualified Data.List.NE               as NE
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
  code <- sendM (readFile file)
  parseInput file code

parsePackage ::
  (Member (Error Failure) effs, LastMember IO effs) => FilePath -> Eff effs Package
parsePackage file = build parseModule file

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
      , "data", "class", "where", "instance"
      , "external"
      , "import"
      , "coerce", "infix"
      , "Int"
      ]

lIdentStart :: Parser Char
lIdentStart = lowerChar

uIdentStart :: Parser Char
uIdentStart = upperChar

identLetter :: Parser Char
identLetter = alphaNumChar <|> satisfy (\c -> c == '_' || c == '\'')

reservedOperators :: Set String
reservedOperators = Set.fromList ["->", "<-", "=>", "=", ":", "|"]

opLetter :: Parser Char
opLetter = oneOf Op.letters

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
uIdent = lexeme $ try $ do
  x <- (:) <$> uIdentStart <*> many identLetter
  when (x `Set.member` reservedIdents) $
    unexpected (Label (NE.fromList ("reserved " ++ x)))
  pure x

binOp :: Parser Op.Binary
binOp = lexeme $ try $ do
  op <- some opLetter
  when (op `Set.member` reservedOperators) $
    unexpected (Label (NE.fromList ("reserved op " ++ op)))
  pure (Op.binary op)

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
evar = Id.evar <$> lIdent <?> "expression variable"

tvar :: Parser Id.TVar
tvar = Id.tvar <$> lIdent <?> "type variable"

tcon :: Parser Id.TCon
tcon = Id.tcon <$> uIdent <?> "type constructor"

dcon :: Parser Id.DCon
dcon = Id.dcon <$> uIdent <?> "data constructor"

clss :: Parser Id.Clss
clss = Id.clss <$> uIdent <?> "class name"

-- TODO: Make 'TAArr' a potential result as well.
typeAtom :: Parser TypeAtom
typeAtom = choice
  [ reserved "Int" $> TAInt
  , TACon <$> tcon
  ]

type_, atype :: Parser Type
type_ =
  makeExprParser
    (mkTApp <$> atype <*> many atype)
    [ [ InfixR (arrow *> pure mkTFun) ] ]
  <?> "data"
atype = choice
  [ TVar <$> tvar
  , TAtm <$> typeAtom
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
defn = indented (lctd evar) $ \z ->
  MkDefn z <$> (mkLam <$> many (lctd evar) <*> (equals *> expr))

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
  (NonEmpty (Defn Id.EVar) -> a) ->
  (NonEmpty (Defn Id.EVar) -> a) ->
  Parser a
let_ mkLet mkRec =
  (reserved "let" *> (reserved "rec" *> pure mkRec <|> pure mkLet))
  <*> NE.sepBy1 defn (reserved "and")

coercion :: Parser Coercion
coercion =
  MkCoercion Inject  <$> (symbol "_" *> arrow *> tcon      ) <|>
  MkCoercion Project <$> (tcon       <* arrow <* symbol "_")

expr, aexpr :: Parser (Expr Id.EVar)
aexpr = choice
  [ EVar <$> evar
  , ECon <$> dcon
  , ENum <$> decimal
  , parens expr
  ]
expr =
  ( fmap ELoc . lctd $
    (flip makeExprParser operatorTable . choice)
    [ mkApp <$> aexpr <*> many aexpr
    , mkIf
      <$> (reserved "if"   *> expr)
      <*> (reserved "then" *> expr)
      <*> (reserved "else" *> expr)
    , exprMatch
    , ELam
      <$> (reserved "fun" *> NE.some (lctd evar))
      <*> (arrow *> expr)
    , let_ ELet ERec <*> (reserved "in" *> expr)
    , ECoe <$> (reserved "coerce" *> char '@' *> parens coercion) <*> aexpr
    ])
  <?> "expression"
  where
    operatorTable = map (map f) (reverse Op.table)
      where
        f MkSpec{_sym, _assoc} =
          g _assoc (try (reservedOp (untag _sym) *> pure (EOpp _sym)))
        g = \case
          Op.AssocLeft  -> InfixL
          Op.AssocRight -> InfixR
          Op.AssocNone  -> InfixN

dataDecl :: Parser TConDecl
dataDecl = indented_ (reserved "data") tconDecl

typeDecl :: Parser TConDecl
typeDecl = indented_ (reserved "type") $
  MkTConDecl
  <$> lctd tcon
  <*> (many tvar <* equals)
  <*> (reserved "external" *> pure (Right []) <|> Left <$> type_)

tconDecl :: Parser TConDecl
tconDecl = MkTConDecl
  <$> lctd tcon
  <*> (many tvar <* equals)
  <*> (Right <$> aligned (some dconDecl))

dconDecl :: Parser DConDecl
dconDecl = indented_ bar (MkDConDecl <$> lctd dcon <*> many atype)

signDecl :: Parser SignDecl
signDecl = indented
  (try (indented (lctd evar) (\z -> colon *> pure z)))
  (\z -> MkSignDecl z <$> typeScheme)

clssDecl :: Parser ClssDecl
clssDecl =
  MkClssDecl <$> lctd clss <*> tvar <*> (reserved "where" *> aligned (many signDecl))

instDecl :: Parser InstDecl
instDecl = do
  q       <- typeCstr
  c       <- lctd clss
  (t, vs) <- (,) <$> typeAtom <*> pure [] <|>
             parens ((,) <$> typeAtom <*> many tvar)
  ds      <- reserved "where" *> aligned (many defn)
  pure (MkInstDecl c t vs q ds)

extnDecl :: Parser ExtnDecl
extnDecl = MkExtnDecl
  <$> lctd evar
  <*> (equals *> char '\"' *> some (lowerChar <|> char '_') <* symbol "\"")

infxDecl :: Parser InfxDecl
infxDecl = MkInfxDecl <$> lctd binOp <*> evar

import_ :: Parser FilePath
import_ = indented_ (reserved "import") $ do
  comps <- lexeme (sepBy1 (some (lowerChar <|> digitChar <|> char '_')) (char '/'))
  pure (Sys.joinPath comps Sys.<.> "pu")

module_ :: FilePath -> Parser Module
module_ file = do
  space
  imps <- many import_
  decls <- many $ choice
    [ DType <$> (typeDecl <|> dataDecl)
    , DClss <$> indented_ (reserved "class") clssDecl
    , DInst <$> indented_ (reserved "instance") instDecl
    , DExtn <$> indented_ (reserved "external") extnDecl
    , DSign <$> signDecl <?> "function declaration"
    , DDefn <$> defn <?> "function definition"
    , DInfx <$> indented_ (reserved "infix") infxDecl
    ]
  pure (MkModule file imps decls)
