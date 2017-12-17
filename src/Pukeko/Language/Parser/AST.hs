-- | AST generated by the parser.
module Pukeko.Language.Parser.AST
  ( -- * Types
    TypeCon
  , ExprCon
  , Module
  , TopLevel (..)
  , Defn
  , Expr (..)
  , Altn (..)
  , Patn

    -- * Smart constructors
  , mkApp
  , mkAppOp
  , mkIf
  , mkLam
  )
where

import           Pukeko.Pos
import           Pukeko.Language.Base.AST (StdDefn (..), StdPatn (..), Bind (..))
import qualified Pukeko.Language.Type     as Ty
import qualified Pukeko.Language.Ident    as Id

type TypeCon = Id.Con
type ExprCon = Id.Con

type Module = [TopLevel]

data TopLevel
  = TypDef Pos [Ty.ADT TypeCon]
  | Val    Pos Id.EVar (Ty.Type TypeCon Ty.Closed)
  | TopLet Pos [Defn Id.EVar]
  | TopRec Pos [Defn Id.EVar]
  | Asm    Pos Id.EVar String

type Defn = StdDefn Expr

data Expr v
  = Var Pos v
  | Con Pos ExprCon
  | Num Pos Int
  | App Pos (Expr v) [Expr v]
  -- | If  Pos (Expr v) (Expr v) (Expr v)
  | Mat Pos (Expr v) [Altn v]
  | Lam Pos [Bind]   (Expr v)
  | Let Pos [Defn v] (Expr v)
  | Rec Pos [Defn v] (Expr v)

data Altn v = MkAltn Pos Patn (Expr v)

type Patn = StdPatn ExprCon

mkApp :: Pos -> Expr v -> [Expr v] -> Expr v
mkApp pos fun args
  | null args = fun
  | otherwise = App pos fun args

mkAppOp :: String -> Pos -> Expr Id.EVar -> Expr Id.EVar -> Expr Id.EVar
mkAppOp sym pos arg1 arg2 =
  let fun = Var pos (Id.op sym)
  in  App pos fun [arg1, arg2]

mkIf :: Pos -> Expr v -> Pos -> Expr v -> Pos -> Expr v -> Expr v
mkIf wt t wu u wv v =
  Mat wt t [ MkAltn wu (Dest wu (Id.constructor "True") []) u
           , MkAltn wv (Dest wv (Id.constructor "False") []) v
           ]

mkLam :: Pos -> [Bind] -> Expr v -> Expr v
mkLam pos patns expr
  | null patns = expr
  | otherwise  = Lam pos patns expr
