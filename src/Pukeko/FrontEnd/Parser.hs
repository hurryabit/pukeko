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
import           Control.Monad.Combinators.Expr
import qualified Control.Monad.RWS                        as RWS
import qualified Data.List.NE                             as NE
import qualified Data.Set                                 as Set
import           System.FilePath                          as Sys
import           Text.Megaparsec                          hiding (parse,
                                                           parseTest)
import qualified Text.Megaparsec                          as MP
import           Text.Megaparsec.Char                     hiding (space)
import qualified Text.Megaparsec.Char.Lexer               as L

import           Pukeko.AST.Name              hiding (Name)
import           Pukeko.AST.Operator          (Spec (..))
import qualified Pukeko.AST.Operator          as Op
import           Pukeko.AST.Surface
import           Pukeko.FrontEnd.Parser.Build (build)

parseInput :: (Member (Error Failure) effs) => FilePath -> String -> Eff effs Module
parseInput file =
  either (throwFailure . pretty . errorBundlePretty) pure .
  parse (module_ file <* eof) file

parseModule ::
  (Member (Error Failure) effs, LastMember IO effs) => FilePath -> Eff effs Module
parseModule file = do
  code <- sendM (readFile file)
  parseInput file code

parsePackage ::
  (Member (Error Failure) effs, LastMember IO effs) => FilePath -> Eff effs Package
parsePackage = build parseModule

type Parser = RWS.RWST (Pos, Ordering) () SourcePos (Parsec Void String)

parse :: Parser a -> FilePath -> String -> Either (ParseErrorBundle String Void) a
parse p file = MP.parse (fst <$> RWS.evalRWST p (pos1, EQ) (initialPos file)) file

parseTest :: (Show a) => Parser a -> String -> IO ()
parseTest p = either (putStrLn . errorBundlePretty) print . parse p ""

getPosition :: MonadParsec e s m => m SourcePos
getPosition = do
  st <- getParserState
  -- We're not interested in the line at which the offset is located in
  -- this case, but the same 'reachOffset' function is used in
  -- 'errorBundlePretty'.
  let (pos, _, pst) = reachOffset (stateOffset st) (statePosState st)
  setParserState st { statePosState = pst }
  return pos

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
      [ "let", "rec", "and", "in"
      , "if", "then", "else"
      , "case", "of"
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
reservedOperators = Set.fromList ["->", "<-", "=>", "=", "::", "|", "\\"]

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

equals, arrow, darrow, hasType, bar, lam :: Parser ()
equals  = reservedOp "="
arrow   = reservedOp "->"
darrow  = reservedOp "=>"
hasType = reservedOp "::"
bar     = reservedOp "|"
lam     = reservedOp "\\"

name :: Parser String -> Parser (LctdName nsp)
name p = lctd (Tagged <$> p)

tmvar :: Parser (LctdName 'TmVar)
tmvar = name lIdent <?> "expression variable"

tyvar :: Parser (LctdName 'TyVar)
tyvar = name lIdent <?> "type variable"

tycon :: Parser (LctdName 'TyCon)
tycon = name uIdent <?> "type constructor"

tmcon :: Parser (LctdName 'TmCon)
tmcon = name uIdent <?> "data constructor"

clasz :: Parser (LctdName 'TyCon)
clasz = name uIdent <?> "class name"

-- TODO: Make 'TAArr' a potential result as well.
typeAtom :: Parser TypeAtom
typeAtom = choice
  [ reserved "Int" $> TAInt
  , TACon <$> tycon
  ]

type_, atype :: Parser Type
type_ =
  makeExprParser
    (mkTApp <$> atype <*> many atype)
    [ [ InfixR (arrow $> mkTFun) ] ]
  <?> "data"
atype = choice
  [ TVar <$> tyvar
  , TAtm <$> typeAtom
  , parens type_
  ]

typeCstr :: Parser TypeCstr
typeCstr = MkTypeCstr
  <$> option [] (try (parens (sepBy1 ((,) <$> clasz <*> tyvar) comma) <* darrow))

typeScheme :: Parser TypeScheme
typeScheme = MkTypeScheme <$> typeCstr <*> type_

-- <patn>  ::= <apatn> | <con> <apatn>*
-- <apatn> ::= '_' | <evar> | <con> | '(' <patn> ')'
patn, apatn :: Parser Patn
patn  = PCon <$> tmcon <*> many apatn <|>
        apatn
apatn = symbol "_" $> PWld   <|>
        PVar <$> tmvar             <|>
        PCon <$> tmcon <*> pure [] <|>
        parens patn

bind :: Parser (Bind (LctdName 'TmVar))
bind = indented tmvar $ \z -> MkBind z <$> (mkLam <$> many tmvar <*> (equals *> expr))

exprMatch :: Parser (Expr (LctdName 'TmVar))
exprMatch = indented_ (reserved "case") $
  EMat
  <$> (expr <* reserved "of")
  <*> aligned (some altn)

altn :: Parser (Altn (LctdName 'TmVar))
altn = indented tmcon $ \con ->
  MkAltn <$> (PCon con <$> many apatn) <*> (arrow *> expr)

let_ ::
  (NonEmpty (Bind (LctdName 'TmVar)) -> a) ->
  (NonEmpty (Bind (LctdName 'TmVar)) -> a) ->
  Parser a
let_ mkLet mkRec =
  (reserved "let" *> (reserved "rec" $> mkRec <|> pure mkLet))
  <*> NE.sepBy1 bind (reserved "and")

coercion :: Parser Coercion
coercion =
  MkCoercion Inject  <$> (symbol "_" *> arrow *> tycon     ) <|>
  MkCoercion Project <$> (tycon      <* arrow <* symbol "_")

expr, aexpr :: Parser (Expr (LctdName 'TmVar))
aexpr = choice
  [ EVar <$> tmvar
  , ECon <$> tmcon
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
      <$> (lam *> NE.some tmvar)
      <*> (arrow *> expr)
    , let_ ELet ERec <*> (reserved "in" *> expr)
    , ECoe <$> (reserved "coerce" *> char '@' *> parens coercion) <*> aexpr
    ])
  <?> "expression"
  where
    operatorTable = map (map f) (reverse Op.table)
      where
        f MkSpec{_sym, _assoc} =
          g _assoc (try (reservedOp (untag _sym) $> EOpp _sym))
        g = \case
          Op.AssocLeft  -> InfixL
          Op.AssocRight -> InfixR
          Op.AssocNone  -> InfixN

dataDecl :: Parser TyConDecl
dataDecl = indented_ (reserved "data") tconDecl

typeDecl :: Parser TyConDecl
typeDecl = indented_ (reserved "type") $
  MkTyConDecl
  <$> tycon
  <*> (many tyvar <* equals)
  <*> (Right [] <$ reserved "external" <|> Left <$> type_)

tconDecl :: Parser TyConDecl
tconDecl = MkTyConDecl
  <$> tycon
  <*> (many tyvar <* equals)
  <*> (Right <$> sepBy1 dconDecl bar)

dconDecl :: Parser TmConDecl
dconDecl = MkTmConDecl <$> tmcon <*> many atype

signDecl :: Parser SignDecl
signDecl = indented
  (try (indented tmvar (\z -> hasType $> z)))
  (\z -> MkSignDecl z <$> typeScheme)

clssDecl :: Parser ClssDecl
clssDecl = do
  super <- optional (parens ((,) <$> clasz <*> tyvar) <* reservedOp "<=")
  clss <- clasz
  tvar <- tyvar
  reserved "where"
  mthds <- aligned (many signDecl)
  pure (MkClssDecl clss tvar super mthds)

instDecl :: Parser InstDecl
instDecl = do
  n       <- tmvar
  hasType
  q       <- typeCstr
  c       <- clasz
  (t, vs) <- (,) <$> typeAtom <*> pure [] <|>
             parens ((,) <$> typeAtom <*> many tyvar)
  ds      <- reserved "where" *> aligned (many bind)
  pure (MkInstDecl n c t vs q ds)

extnDecl :: Parser ExtnDecl
extnDecl = MkExtnDecl
  <$> tmvar
  <*> (equals *> char '\"' *> some (lowerChar <|> char '_') <* symbol "\"")

infxDecl :: Parser InfxDecl
infxDecl = MkInfxDecl <$> lctd binOp <*> tmvar

import_ :: Parser FilePath
import_ = indented_ (reserved "import") $
  Sys.joinPath
  <$> lexeme (sepBy1 (some (lowerChar <|> digitChar <|> char '_')) (char '/'))

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
    , DDefn <$> bind <?> "function definition"
    , DInfx <$> indented_ (reserved "infix") infxDecl
    ]
  pure (MkModule file imps decls)
