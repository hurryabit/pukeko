module Pukeko.Language.TypeEraser
  ( eraseModule
  ) where

import           Pukeko.Language.AST.Std
import qualified Pukeko.Language.AST.Stage as St
import           Pukeko.Language.Type      (NoType (..))

type In  = St.DeadCode
type Out = St.TypeEraser

eraseModule :: Module In -> Module Out
eraseModule (MkModule info tops) = MkModule info (map eraseTopLevel tops)

eraseTopLevel :: TopLevel In -> TopLevel Out
eraseTopLevel = \case
  TLDef b e -> TLDef (eraseBind b) (eraseExpr e)
  TLAsm b s -> TLAsm (eraseBind b) s

eraseDefn :: Defn In tv ev -> Defn Out Void ev
eraseDefn (MkDefn b e) = MkDefn (eraseBind b) (eraseExpr e)

eraseExpr :: Expr In tv ev -> Expr Out Void ev
eraseExpr = \case
  EVar w x      -> EVar w x
  ECon w c      -> ECon w c
  ENum w n      -> ENum w n
  EApp w e0 es  -> EApp w (eraseExpr e0) (map eraseExpr es)
  ELam w bs e0  -> ELam w (fmap eraseBind bs) (eraseExpr e0)
  ELet w ds e0  -> ELet w (fmap eraseDefn ds) (eraseExpr e0)
  ERec w ds e0  -> ERec w (fmap eraseDefn ds) (eraseExpr e0)
  ECas w e0 cs  -> ECas w (eraseExpr e0) (map eraseCase cs)
  ETyAbs _ _ e0 -> eraseExpr e0
  ETyApp _ e0 _ -> eraseExpr e0

eraseBind :: Bind In tv -> Bind Out Void
eraseBind (MkBind w x _) = MkBind w x NoType

eraseCase :: Case In tv ev -> Case Out Void ev
eraseCase (MkCase w c xs e) = MkCase w c xs (eraseExpr e)
