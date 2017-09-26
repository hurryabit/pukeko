module Pukeko.Language.KindChecker.AST
  ( Module
  , TopLevel (..)
  , module Pukeko.Language.TypeResolver.AST
  )
where

import           Pukeko.Language.Base.AST
import           Pukeko.Language.TypeResolver.AST hiding (Module, TopLevel (..))
import qualified Pukeko.Language.Ident            as Id
import qualified Pukeko.Language.Type             as Ty

type Module = [TopLevel]

data TopLevel
  =           Val    Pos Id.EVar (Ty.Type TypeCon Ty.Closed)
  | forall n. TopLet Pos (Vector n (Defn Id.EVar))
  | forall n. TopRec Pos (Vector n (Defn (FinScope n Id.EVar)))
  |           Asm    Pos Id.EVar String
