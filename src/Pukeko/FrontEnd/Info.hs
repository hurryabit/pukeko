{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Pukeko.FrontEnd.Info
  ( ModuleInfo
  , HasModuleInfo
  , SomeInstDecl (..)
  , info2tycons
  , info2tmcons
  , info2signs
  , info2classes
  , info2methods
  , info2insts
  , info2dicts
  , runInfo
  , lookupInfo
  , findInfo
  , typeOfFunc
  , typeOfAtom
  , tyconDeclInfo
  ) where

import Pukeko.Prelude

import           Control.Lens                (foldMapOf, _Right)
import qualified Data.Map.Extended           as Map
import qualified Pukeko.AST.SystemF    as SysF
import qualified Pukeko.AST.SuperCore  as Core
import           Pukeko.AST.Language
import           Pukeko.AST.Name
import           Pukeko.AST.ConDecl
import           Pukeko.AST.Expr
import           Pukeko.AST.Type


data ModuleInfo = MkModuleInfo
  { _info2tycons  :: Map TyCon TyConDecl
  , _info2tmcons  :: Map TmCon (TyConDecl, TmConDecl)
  , _info2signs   :: Map TmVar Type
  , _info2classes :: Map Class SysF.ClassDecl
  , _info2methods :: Map TmVar (SysF.ClassDecl, SysF.SignDecl TyVar)
  , _info2insts   :: Map (Class, TypeAtom) SomeInstDecl
  , _info2dicts   :: Map DxVar SomeInstDecl
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
findInfo l k = views l (Map.! k)

typeOfFunc :: (HasCallStack, Member (Reader ModuleInfo) effs) => TmVar -> Eff effs Type
typeOfFunc = findInfo info2signs

typeOfAtom :: (HasCallStack, Member (Reader ModuleInfo) effs) => Atom -> Eff effs Type
typeOfAtom = \case
  AVal z -> typeOfFunc z
  ACon c -> uncurry typeOfTmCon <$> findInfo info2tmcons c
  ANum _ -> pure (TAtm TAInt)

itemInfo :: (Ord k) => Lens' ModuleInfo (Map k v) -> k -> v -> ModuleInfo
itemInfo l k v = set l (Map.singleton k v) mempty

tyconDeclInfo :: TyConDecl -> ModuleInfo
tyconDeclInfo tcon =
  let dis = foldMapOf
        (tycon2tmcons . _Right . traverse)
        (\dcon -> itemInfo info2tmcons (nameOf dcon) (tcon, dcon))
        tcon
  in  itemInfo info2tycons (nameOf tcon) tcon <> dis

-- FIXME: Detect multiple definitions.
signInfo :: TmVar -> Type -> ModuleInfo
signInfo = itemInfo info2signs

instance IsType (TypeOf st) => HasModuleInfo (SysF.Module st) where
  collectInfo (SysF.MkModule decls) = foldFor decls $ \case
    SysF.DType tcon -> tyconDeclInfo tcon
    SysF.DSign (SysF.MkSignDecl func typ_) -> signInfo func typ_
    -- NOTE: See the comment for 'SysF._func2type' on why we treat 'TArr' like this.
    -- SysF.DFunc (SysF.MkFuncDecl _    TArr _) -> mempty
    SysF.DFunc (SysF.MkFuncDecl func typ_ _) ->
      maybe mempty (signInfo func) (isType typ_)
    -- SysF.DExtn (SysF.MkExtnDecl _    TArr _) -> mempty
    SysF.DExtn (SysF.MkExtnDecl func typ_ _) ->
      maybe mempty (signInfo func) (isType typ_)
    SysF.DClss clssDecl@(SysF.MkClassDecl clss param _super _dcon mthds) ->
      let mthds_info = foldFor mthds $ \mthdDecl@(SysF.MkSignDecl mthd typ0) ->
            let typ1 = TUni' param (TCtx (clss, TVar param) typ0)
            in  signInfo mthd typ1
                <> itemInfo info2methods mthd (clssDecl, mthdDecl)
      in  itemInfo info2classes clss clssDecl <> mthds_info
    SysF.DInst inst0@(SysF.MkInstDecl dict clss t _ _ _ _) ->
      let inst1 = SomeInstDecl inst0
      in  itemInfo info2insts (clss, t) inst1 <> itemInfo info2dicts dict inst1

instance HasModuleInfo Core.Module where
  collectInfo (Core.MkModule types extns supcs) = fold
    [ foldMap tyconDeclInfo types
    , foldMap (\extn -> signInfo (nameOf extn) (vacuous (Core._extn2type extn))) extns
    , foldMap (\supc -> signInfo (nameOf supc) (vacuous (Core._supc2type supc))) supcs
    ]

foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap

instance Semigroup ModuleInfo where
  MkModuleInfo a1 b1 c1 d1 e1 f1 g1 <> MkModuleInfo a2 b2 c2 d2 e2 f2 g2 =
    MkModuleInfo (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2) (e1 <> e2) (f1 <> f2) (g1 <> g2)

instance Monoid ModuleInfo where
  mempty = MkModuleInfo mempty mempty mempty mempty mempty mempty mempty
