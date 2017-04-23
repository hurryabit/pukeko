{-# LANGUAGE TemplateHaskell #-}
module Pukeko.GMachine.Compiler
  ( compile
  , Program
  )
  where

import Control.Monad.Except
import Control.Monad.RWS hiding (asks, local)
import Data.Label.Derive
import Data.Label.Monadic
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import Pukeko.GMachine.GCode
import Pukeko.Language.ADT
import Pukeko.Language.Syntax

import qualified Pukeko.GMachine.GCode    as GCode
import qualified Pukeko.GMachine.Builtins as Builtins

data Context = MkContext
  { _offsets :: Map Ident Int
  , _depth   :: Int
  , _constrs :: Map Ident Constructor
  }
mkLabels [''Context]

newtype CC a = CC { unCC :: ExceptT String (RWS Context [Inst] Int) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader Context
           , MonadWriter [Inst]
           )

freshLabel :: CC Name
freshLabel = CC $ state $ \n -> (Name ('.':show n), n+1)

compile :: MonadError String m => Map Ident Constructor -> Expr () -> m Program
compile constrs Let{ _isrec = True, _defns, _body = Var{ _ident }} = do
  globals <- mapM (compileDefn constrs) _defns
  let all_globals = Builtins.everything (Map.elems constrs) ++ globals
      name_to_global =
        Map.fromList $ map (\global -> (GCode._name global, global)) all_globals
      deps name =
        case Map.lookup name name_to_global of
          Nothing     -> throwError $ "Unknown global: " ++ unName name
          Just global -> return $ dependencies global
  let _main = name _ident
  reachable_names <- saturate deps _main
  let _globals =
        filter (\global -> GCode._name global `Set.member` reachable_names) all_globals
  return $ MkProgram { _globals, _main }
compile _ _ = throwError "compile expects output of lambda lifter"

findConstructor :: Ident -> CC Constructor
findConstructor ident = do
  _constrs <- asks constrs
  case Map.lookup ident _constrs of
    Nothing -> throwError $ "unknown constructor: " ++ show ident
    Just constr -> return constr

compileDefn :: MonadError String m => Map Ident Constructor -> Defn a -> m Global
compileDefn _constrs MkDefn{ _bind = MkBind{ _ident }, _expr } = do
  let (binds, body) =
        case _expr of
          Lam { _binds, _body } -> (_binds, _body)
          _                     -> ([]    , _expr)
      n = length binds
      (idents, _) = unzipBinds binds
      context = MkContext
        { _offsets = Map.fromList (zip idents [n, n-1 ..])
        , _depth   = n
        , _constrs
        }
      (res, code) = evalRWS (runExceptT (unCC $ ccExpr Redex body)) context 0
  case res of
    Left error -> throwError error
    Right ()   -> do
      let _name  = name _ident
          _arity = n
          _code  = GLOBSTART _name _arity : code
      return $ MkGlobal { _name, _arity, _code }

data Mode = Stack | Eval | Redex

whenStackOrEval, whenRedex :: Mode -> CC () -> CC ()
whenStackOrEval mode cc =
  case mode of
    Stack -> cc
    Eval  -> cc
    Redex -> return ()
whenRedex mode cc =
  case mode of
    Stack -> return ()
    Eval  -> return ()
    Redex -> cc

continueRedex :: Inst -> CC ()
continueRedex inst = do
  d <- asks depth
  tell [UPDATE (d+1), POP d, inst]

ccExpr :: Mode -> Expr a -> CC ()
ccExpr mode expr =
  case expr of
    Var { _ident } -> do
      MkContext { _offsets, _depth } <- ask
      case Map.lookup _ident _offsets of
        Nothing     -> tell [PUSHGLOBAL (name _ident)]
        Just offset -> tell [PUSH (_depth - offset)]
      case mode of
        Stack -> return ()
        Eval  -> tell [EVAL]
        Redex -> continueRedex UNWIND
    Num { _int } -> do
      tell [PUSHINT _int]
      whenRedex mode $ continueRedex RETURN
    Ap{ _fun, _args } -> do
      let n = length _args
      constr_tag <-
        case _fun of
          Var{ _ident }
            | isConstructor _ident -> do
                MkConstructor{ _tag, _fields } <- findConstructor _ident
                case length _fields `compare` n of
                  LT -> throwError $
                    "constructor " ++ show _ident ++ " has too many arguments"
                  EQ | n > 0 -> return (Just _tag)
                  _          -> return Nothing
          _ -> return Nothing
      let ccExprAt i expr = local depth (+i) $ ccExpr Stack expr
      zipWithM_ ccExprAt [0..] (reverse _args)
      case constr_tag of
        Just tag -> do
          tell [CONS tag n]
          case mode of
            Stack -> return ()
            Eval  -> return ()
            Redex -> continueRedex RETURN
        Nothing -> do
          ccExprAt n _fun
          tell [MKAP n]
          case mode of
            Stack -> return ()
            Eval  -> tell [EVAL]
            Redex -> continueRedex UNWIND
    ApOp{ _op, _arg1, _arg2 } -> do
      let eval continue = do
            case lookup _op Builtins.binops of
              Nothing -> ccExpr mode $ desugarApOp expr
              Just (inst, _) -> do
                ccExpr Eval _arg2
                local depth succ $ ccExpr Eval _arg1
                tell [inst]
                continue
      case mode of
        Stack -> ccExpr mode $ desugarApOp expr
        Eval  -> eval $ return ()
        Redex -> eval $ continueRedex RETURN
    Lam { } -> throwError "All lambdas should be lifted by now"
    Let { _isrec = False, _defns, _body } -> do
      let n = length _defns
          (idents, _, rhss) = unzipDefns3 _defns
      zipWithM_ (\rhs k -> local depth (+k) $ ccExpr Stack rhs) rhss [0 ..]
      localDecls idents $ ccExpr mode _body
      whenStackOrEval mode $ tell [SLIDE n]
    Let { _isrec = True, _defns, _body } -> do
      let n = length _defns
          (idents, _, rhss) = unzipDefns3 _defns
      tell [ALLOC n]
      localDecls idents $ do
        zipWithM_ (\rhs k -> ccExpr Stack rhs >> tell [UPDATE (n-k)]) rhss [0 ..]
        ccExpr mode _body
      whenStackOrEval mode $ tell [SLIDE n]
    If { } -> ccExpr mode $ desugarIf expr
    Match{ _expr, _altns } -> do
      ccExpr Eval _expr
      let MkAltn{ _cons }:_ = _altns
      MkConstructor{ _adt = MkADT{ _constructors } } <- findConstructor _cons
      case _constructors of
        [c] -> ccAltn mode _altns c
        cs -> do
          ls <- mapM (\_ -> freshLabel) cs
          done <- freshLabel
          tell [JUMPCASE ls]
          forM_ (zip ls cs) $ \(l, c) -> do
            tell [LABEL l]
            ccAltn mode _altns c
            tell [JUMP done]
          tell [LABEL done]

ccAltn :: Mode -> [Altn a] -> Constructor -> CC ()
ccAltn mode altns MkConstructor{ _name } =
  case List.find (\MkAltn{ _cons } -> _cons == _name) altns of
    Nothing -> throwError $ "match statement does not mention " ++ show _name
    Just MkAltn{ _binds, _rhs } -> do
      let arity = length _binds
          (idents, _) = unzipBinds _binds
      tell [UNCONS arity]
      localDecls (reverse idents) (ccExpr mode _rhs)
      whenStackOrEval mode $ tell [SLIDE arity]


localDecls :: [Ident] -> CC a -> CC a
localDecls idents cc = do
  let n = length idents
  d <- asks depth
  let offs = Map.fromList (zip idents [d+1 ..])
  local offsets (Map.union offs) $ local depth (+n) $ cc

name :: Ident -> Name
name ident = Name (unIdent ident)


saturate :: (Monad m, Ord a) => (a -> m (Set a)) -> a -> m (Set a)
saturate f x0 = run [x0] Set.empty
  where
    run []     visited = return visited
    run (x:xs) visited
      | x `Set.member` visited = run xs visited
      | otherwise              = do
          ys <- f x
          run (Set.toList ys ++ xs) (Set.insert x visited)
