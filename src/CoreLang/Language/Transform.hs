module CoreLang.Language.Transform
  ( bottomUp
  , bottomUpM
  , topDown
  , topDownM
  , Transformation (_preExpr, _postExpr, _preDefn, _postDefn, _prePatn, _postPatn)
  , emptyTransformation
  , transform
  )
  where

import Control.Monad.Identity

import CoreLang.Language.Syntax

bottomUp :: (Expr a -> Expr a)
         -> (Defn a -> Defn a)
         -> (Patn a -> Patn a)
         -> (Expr a -> Expr a)
bottomUp postExpr postDefn postPatn =
  runIdentity .
    bottomUpM (Identity . postExpr) (Identity . postDefn) (Identity . postPatn)

bottomUpM :: Monad m => (Expr a -> m (Expr a))
                     -> (Defn a -> m (Defn a))
                     -> (Patn a -> m (Patn a))
                     -> (Expr a -> m (Expr a))
bottomUpM _postExpr _postDefn _postPatn =
  transform $ emptyTransformation { _postExpr, _postDefn, _postPatn }

topDown :: (Expr a -> Expr a)
        -> (Defn a -> Defn a)
        -> (Patn a -> Patn a)
        -> (Expr a -> Expr a)
topDown preExpr preDefn prePatn =
  runIdentity .
    bottomUpM (Identity . preExpr) (Identity . preDefn) (Identity . prePatn)

topDownM :: Monad m => (Expr a -> m (Expr a))
                    -> (Defn a -> m (Defn a))
                    -> (Patn a -> m (Patn a))
                    -> (Expr a -> m (Expr a))
topDownM _preExpr _preDefn _prePatn =
  transform $ emptyTransformation { _preExpr, _preDefn, _prePatn }

data Transformation m a = MkTransformation
  { _preExpr  :: Expr a -> m (Expr a)
  , _postExpr :: Expr a -> m (Expr a)
  , _preDefn  :: Defn a -> m (Defn a)
  , _postDefn :: Defn a -> m (Defn a)
  , _prePatn  :: Patn a -> m (Patn a)
  , _postPatn :: Patn a -> m (Patn a)
  }

emptyTransformation :: Monad m => Transformation m a
emptyTransformation = MkTransformation
  { _preExpr  = return
  , _postExpr = return
  , _preDefn  = return
  , _postDefn = return
  , _prePatn  = return
  , _postPatn = return
  }

transform :: Monad m => Transformation m a -> Expr a -> m (Expr a)
transform MkTransformation{ _preExpr, _postExpr, _preDefn, _postDefn, _prePatn, _postPatn } =
  transExpr
  where
    transExpr expr = do
      expr <- _preExpr expr
      expr <-
        case expr of
          Var { }  -> return expr
          Num { }  -> return expr
          Pack { } -> return expr
          Ap { _fun, _arg } -> do
            _fun <- transExpr _fun
            _arg <- transExpr _arg
            return $ expr { _fun, _arg }
          ApOp { _arg1, _arg2 } -> do
            _arg1 <- transExpr _arg1
            _arg2 <- transExpr _arg2
            return $ expr { _arg1, _arg2 }
          Let { _defns, _body } -> do
            _defns <- mapM transDefn _defns
            _body  <- transExpr _body
            return $ expr { _defns, _body }
          Lam { _patns, _body } -> do
            _patns <- mapM transPatn _patns
            _body  <- transExpr _body
            return $ expr { _patns, _body }
          If { _cond, _then, _else } -> do
            _cond <- transExpr _cond
            _then <- transExpr _then
            _else <- transExpr _else
            return $ expr { _cond, _then, _else }
          Rec { _defns } -> do
            _defns <- mapM transDefn _defns
            return $ expr { _defns }
          Sel { _expr } -> do
            _expr <- transExpr _expr
            return $ (expr { _expr } :: Expr _)
      _postExpr expr
    transDefn defn = do
      defn@MkDefn{ _patn , _expr } <- _preDefn defn
      _patn <- transPatn _patn
      _expr <- transExpr _expr
      _postDefn $ defn { _patn, _expr }
    transPatn patn = do
      patn <- _prePatn patn
      _postPatn patn
