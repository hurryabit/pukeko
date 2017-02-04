module CoreLang.Language.Transform
  ( bottomUp
  , bottomUpM
  )
  where

import Control.Monad.Identity

import CoreLang.Language.Syntax

bottomUp :: (Expr a -> Expr a)
         -> (Defn a -> Defn a)
         -> (Patn a -> Patn a)
         -> (Expr a -> Expr a)
bottomUp f_expr f_defn f_patn =
  runIdentity . bottomUpM (Identity . f_expr) (Identity . f_defn) (Identity . f_patn)

bottomUpM :: Monad m => (Expr a -> m (Expr a))
                     -> (Defn a -> m (Defn a))
                     -> (Patn a -> m (Patn a))
                     -> (Expr a -> m (Expr a))
bottomUpM f_expr f_defn f_patn = bu_expr
  where
    bu_expr expr =
      case expr of
        Var { }  -> f_expr expr
        Num { }  -> f_expr expr
        Pack { } -> f_expr expr
        Ap { _fun, _arg } -> do
          _fun <- bu_expr _fun
          _arg <- bu_expr _arg
          f_expr $ expr { _fun, _arg }
        Let { _defns, _body } -> do
          _defns <- mapM bu_defn _defns
          _body  <- bu_expr _body
          f_expr $ expr { _defns, _body }
        LetRec { _defns, _body } -> do
          _defns <- mapM bu_defn _defns
          _body  <- bu_expr _body
          f_expr $ expr { _defns, _body }
        Lam { _patns, _body } -> do
          _patns <- mapM bu_patn _patns
          _body  <- bu_expr _body
          f_expr $ expr { _patns, _body }
        If { _cond, _then, _else } -> do
          _cond <- bu_expr _cond
          _then <- bu_expr _then
          _else <- bu_expr _else
          f_expr $ expr { _cond, _then, _else }
        Rec { _defns } -> do
          _defns <- mapM bu_defn _defns
          f_expr $ expr { _defns }
        Sel { _expr } -> do
          _expr <- bu_expr _expr
          f_expr $ expr { _expr }
    bu_defn defn@MkDefn { _patn , _expr } = do
      _patn <- bu_patn _patn
      _expr <- bu_expr _expr
      f_defn $ defn { _patn, _expr }
    bu_patn = f_patn
