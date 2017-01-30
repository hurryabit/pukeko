module CoreLang.Monomorphic.Checker
  ( checkExpr
  )
  where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map (Map)

import qualified Data.Map as Map

import CoreLang.Pretty
import CoreLang.Language.Syntax
import CoreLang.Language.Type (Type, (~>), record)

import qualified CoreLang.Language.Type        as Type
import qualified CoreLang.Monomorphic.Builtins as Builtins


checkExpr :: MonadError String m => Expr -> m Type
checkExpr expr = runTC (Map.fromList Builtins.everything) (check expr)


type Environment = Map Ident Type

newtype TC a =
  TC  { unTC :: ExceptT String (Reader Environment) a }
  deriving ( Functor, Applicative, Monad
           , MonadError  String
           , MonadReader Environment
           )

runTC :: MonadError String m => Environment -> TC a -> m a
runTC env tc =
  case runReader (runExceptT (unTC tc)) env of
    Left  e -> throwError e
    Right x -> return x

match :: Type -> Type -> TC ()
match expected found
  | expected == found = return ()
  | otherwise         = pthrow (text "expected" <+> pretty expected <> text ", but found" <+> pretty found)

matchMaybe :: Maybe Type -> Type -> TC ()
matchMaybe expected found = forM_ expected (`match` found)

check :: Expr -> TC Type
check expr =
  case expr of
    Var { _ident } -> do
      env <- ask
      case Map.lookup _ident env of
        Nothing -> pthrow (text "unknown identifier:" <+> pretty _ident)
        Just  t -> return t
    Num { } -> return Type.int
    Ap { _fun, _arg } -> do
      t_fun <- check _fun
      case t_fun of
        Type.Fun t_x t_y -> do
          t_arg <- check _arg
          match t_x t_arg
          return t_y
        _ -> pthrow (text "expected function, found" <+> pretty t_fun)
    Lam { _decls, _body } -> do
      (t_decls, t_body) <- localDecls _decls (check _body)
      return $ foldr (~>) t_body t_decls
    Let { _defns, _body } -> do
      t_defns <- checkDefns _defns
      let env = Map.fromList t_defns
      local (Map.union env) (check _body)
    LetRec { _defns, _body } -> do
      (_, t_body) <- localDecls (map _decl _defns) $ check (Let { _defns, _body })
      return t_body
    If { _cond, _then, _else } -> do
      t_cond <- check _cond
      match Type.bool t_cond
      t_then <- check _then
      t_else <- check _else
      match t_then t_else
      return t_then
    Rec { _defns } -> do
      t_defns <- checkDefns _defns
      return (record t_defns)
    Sel { _expr, _field } -> do
      t_expr <- check _expr
      let mismatch = pthrow (text "expected record type with field" <+> pretty _field <> text ", found" <+> pretty t_expr)
      case t_expr of
        Type.Rec fields ->
          case lookup _field fields of
            Nothing      -> mismatch
            Just t_field -> return t_field
        _ -> mismatch
    Pack { } -> pthrow (text "type checking constructors not implemented")

checkDefns :: [Defn] -> TC [(Ident, Type)]
checkDefns defns =
  forM defns $ \MkDefn { _decl = MkDecl { _ident, _type }, _expr } -> do
    t_expr <- check _expr
    matchMaybe _type t_expr
    return (_ident, t_expr)

localDecls :: [Decl] -> TC a -> TC ([Type], a)
localDecls decls tc = do
  env_list <- forM decls $ \MkDecl { _ident, _type } ->
    case _type of
      Nothing     -> pthrow (pretty _ident <+> text "lacks a type annotation")
      Just t_decl -> return (_ident, t_decl)
  let env = Map.fromList env_list
  res <- local (Map.union env) tc
  return (map snd env_list, res)
