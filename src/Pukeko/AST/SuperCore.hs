module Pukeko.AST.SuperCore
  ( module Pukeko.AST.SuperCore
  , module Pukeko.AST.SystemF
  ) where

import Pukeko.Prelude

import qualified Data.List.NE as NE
import qualified Data.Map as Map

import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Language
import           Pukeko.AST.SystemF
import           Pukeko.AST.Type

data ModuleSC = MkModuleSC
  { _modsc2types :: Map Id.TCon TConDecl
  , _modsc2extns :: Map Id.EVar (ExtnDecl Type)
  , _modsc2supcs :: Map Id.EVar SupCDecl
  }

type DefnSC = Defn SuperCore
type ExprSC = Expr SuperCore
type BindSC = Bind Type
type CaseSC = Case SuperCore

makeLenses ''ModuleSC

toModule :: ModuleSC -> Module SuperCore
toModule (MkModuleSC types extns supcs) =
  let decls = concat
        [ map (DType . NE.singleton) (toList types)
        , map DExtn (toList extns)
        , map DSupC (toList supcs)
        ]
  in  MkModule decls

mkDecl :: (Ord k) => Lens' ModuleSC (Map k v) -> (Lens' v (Lctd k)) -> v -> ModuleSC
mkDecl l k v = over l (Map.insert (v^.k.lctd) v) mempty

mkTypeDecl :: TConDecl -> ModuleSC
mkTypeDecl = mkDecl modsc2types tcon2name

mkExtnDecl :: ExtnDecl Type -> ModuleSC
mkExtnDecl = mkDecl modsc2extns (extn2bind.bind2evar)

mkSupCDecl :: SupCDecl -> ModuleSC
mkSupCDecl = mkDecl modsc2supcs supc2func

instance Semigroup ModuleSC where
  MkModuleSC t1 e1 s1 <> MkModuleSC t2 e2 s2 =
    MkModuleSC (t1 <> t2) (e1 <> e2) (s1 <> s2)

instance Monoid ModuleSC where
  mempty = MkModuleSC mempty mempty mempty
  mappend = (<>)

instance Pretty ModuleSC where
  pretty = pretty . toModule
