{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Info
  ( ModuleInfo
  , HasModuleInfo
  , SomeInstDecl (..)
  , info2tcons
  , info2dcons
  , info2signs
  , info2clsss
  , info2mthds
  , info2insts
  , runInfo
  , lookupInfo
  , findInfo
  , typeOfFunc
  , typeOfAtom
  , tconDeclInfo
  ) where

import Pukeko.Prelude

import           Control.Lens                (foldMapOf, _Right)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set

import qualified Pukeko.AST.SystemF    as SysF
import qualified Pukeko.AST.SuperCore  as Core
import           Pukeko.AST.Language
import           Pukeko.AST.ConDecl
import qualified Pukeko.AST.Identifier as Id
import           Pukeko.AST.Expr
import           Pukeko.AST.Type


data ModuleInfo = MkModuleInfo
  { _info2tcons :: Map Id.TCon TConDecl
  , _info2dcons :: Map Id.DCon (TConDecl, DConDecl)
  , _info2signs :: Map Id.EVar (Type Void)
  , _info2clsss :: Map Id.Clss SysF.ClssDecl
  , _info2mthds :: Map Id.EVar (SysF.ClssDecl, Bind Type (TScope Int Void))
  , _info2insts :: Map (Id.Clss, Id.TCon) SomeInstDecl
  }

data SomeInstDecl = forall st. SomeInstDecl (SysF.InstDecl st)

class HasModuleInfo m where
  collectInfo :: m -> ModuleInfo

makeLenses ''ModuleInfo
makePrisms ''ModuleInfo

runInfo :: (HasModuleInfo m) => m -> Eff (Reader ModuleInfo : effs) a -> Eff effs a
runInfo = runReader . collectInfo

lookupInfo ::
  (Member (Reader ModuleInfo) effs, Ord k) =>
  Lens' ModuleInfo (Map k v) -> k -> Eff effs (Maybe v)
lookupInfo l k = Map.lookup k <$> view l

findInfo ::
  (HasCallStack, Member (Reader ModuleInfo) effs, Ord k, Show k) =>
  Lens' ModuleInfo (Map k v) -> k -> Eff effs v
findInfo l k = Map.findWithDefault (bugWith "findInfo" k) k <$> view l

typeOfFunc ::
  (HasCallStack, Member (Reader ModuleInfo) effs) =>
  Id.EVar -> Eff effs (Type tv)
typeOfFunc func = fmap absurd <$> findInfo info2signs func

typeOfAtom :: (HasCallStack, Member (Reader ModuleInfo) effs) =>
  Atom -> Eff effs (Type tv)
typeOfAtom = \case
  AVal z -> typeOfFunc z
  ACon c -> fmap absurd <$> uncurry typeOfDCon <$> findInfo info2dcons c
  ANum _ -> pure typeInt

itemInfo :: (Ord k) => Lens' ModuleInfo (Map k v) -> k -> v -> ModuleInfo
itemInfo l k v = set l (Map.singleton k v) mempty

tconDeclInfo :: TConDecl -> ModuleInfo
tconDeclInfo tcon =
  let dis = foldMapOf
        (tcon2dcons . _Right . traverse)
        (\dcon -> itemInfo info2dcons (dcon^.dcon2name.lctd) (tcon, dcon))
        tcon
  in  itemInfo info2tcons (tcon^.tcon2name.lctd) tcon <> dis

-- FIXME: Detect multiple definitions.
bindInfo :: IsType ty => Lens' decl (Bind ty Void) -> decl -> ModuleInfo
bindInfo l ((^.l) -> MkBind z t) =
  maybe mempty (itemInfo info2signs (unlctd z)) (isType t)

instance IsType (TypeOf st) => HasModuleInfo (SysF.Module st) where
  collectInfo (SysF.MkModule decls) = foldFor decls $ \case
    SysF.DType tcons -> foldMap tconDeclInfo tcons
    SysF.DSign (MkBind z t) -> bindInfo id (MkBind z t)
    SysF.DClss clss@(SysF.MkClssDecl (unlctd -> c) v ms) ->
      let mthds_info = foldFor ms $ \mthd@(MkBind z t0) ->
            let t1 = mkTUni [MkQVar (Set.singleton c) v] t0
            in  bindInfo id (MkBind z t1)
                <> itemInfo info2mthds (z^.lctd) (clss, mthd)
      in  itemInfo info2clsss c clss <> mthds_info
    SysF.DInst inst@(SysF.MkInstDecl (unlctd -> c) t _ _) ->
      itemInfo info2insts (c, t) (SomeInstDecl inst)
    SysF.DDefn defn -> bindInfo defn2bind defn
    SysF.DExtn extn -> bindInfo SysF.extn2bind extn

instance HasModuleInfo Core.Module where
  collectInfo (Core.MkModule types extns supcs) = fold
    [ foldMap tconDeclInfo types
    , foldMap (bindInfo Core.func2bind) extns
    , foldMap (bindInfo Core.func2bind) supcs
    ]

foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap

instance Semigroup ModuleInfo where
  MkModuleInfo a1 b1 c1 d1 e1 f1 <> MkModuleInfo a2 b2 c2 d2 e2 f2 =
    MkModuleInfo (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2)

instance Monoid ModuleInfo where
  mempty = MkModuleInfo mempty mempty mempty mempty mempty mempty
  mappend = (<>)
