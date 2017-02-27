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
import Pukeko.Language.Builtins
import Pukeko.Language.Syntax

import qualified Pukeko.GMachine.GCode    as GCode
import qualified Pukeko.GMachine.Builtins as Builtins

data Context = MkContext
  { _offsets :: Map Ident Int
  , _depth   :: Int
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

compile :: MonadError String m => Expr () -> m Program
compile expr =
  case expr of
    Let { _isrec = True, _defns, _body = Var { _ident } } -> do
      let _main = Name (unIdent _ident)
      globals <- mapM compileDefn _defns
      let all_globals = Builtins.everything ++ globals
          name_to_global =
            Map.fromList $ map (\global -> (GCode._name global, global)) all_globals
          deps name =
            case Map.lookup name name_to_global of
              Nothing     -> throwError $ "Unknown global: " ++ unName name
              Just global -> return $ dependencies global
      reachable_names <- saturate deps _main
      let _globals =
            filter (\global -> GCode._name global `Set.member` reachable_names) all_globals
      return $ MkProgram { _globals, _main }
    _ -> throwError "Expression is not generated by lambda lifter."


compileDefn :: MonadError String m => Defn a -> m Global
compileDefn MkDefn{ _patn = MkPatn{ _ident }, _expr } = do
  let (patns, body) =
        case _expr of
          Lam { _patns, _body } -> (_patns, _body)
          _                     -> ([]    , _expr)
      n = length patns
      (idents, _) = unzipPatns patns
      context = MkContext
        { _offsets = Map.fromList (zip idents [n, n-1 ..])
        , _depth   = n
        }
      (res, code) = evalRWS (runExceptT (unCC $ ccExpr Redex body)) context 0
  case res of
    Left error -> throwError error
    Right ()   -> do
      let _name  = name _ident
          _arity = n
          _code  = GLOBSTART _name _arity : code
      return $ MkGlobal { _name, _arity, _code }

data Mode = Stack | Redex

whenStack, whenRedex :: Mode -> CC () -> CC ()
whenStack Stack cc = cc
whenStack _     _  = return ()
whenRedex Redex cc = cc
whenRedex _     _  = return ()

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
      whenRedex mode $ do
        -- TODO: This is not necessary if we've just pushed a non-CAF global
        tell [EVAL]
        continueRedex UNWIND
    Num { _int } -> do
      tell [PUSHINT _int]
      whenRedex mode $ continueRedex RETURN
    Ap { _fun, _args } -> do
      let ccExprAt i expr = local depth (+i) $ ccExpr Stack expr
      zipWithM_ ccExprAt [0..] (reverse (_fun:_args))
      tell [MKAP (length _args)]
      whenRedex mode $ continueRedex UNWIND
    ApOp { } -> ccExpr mode $ desugarApOp expr
    Lam { } -> throwError "All lambdas should be lifted by now"
    Let { _isrec = False, _defns, _body } -> do
      let n = length _defns
          (idents, _, rhss) = unzipDefns3 _defns
      zipWithM_ (\rhs k -> local depth (+k) $ ccExpr Stack rhs) rhss [0 ..]
      localDecls idents $ ccExpr mode _body
      whenStack mode $ tell [SLIDE n]
    Let { _isrec = True, _defns, _body } -> do
      let n = length _defns
          (idents, _, rhss) = unzipDefns3 _defns
      tell [ALLOC n]
      localDecls idents $ do
        zipWithM_ (\rhs k -> ccExpr Stack rhs >> tell [UPDATE (n-k)]) rhss [0 ..]
        ccExpr mode _body
      whenStack mode $ tell [SLIDE n]
    Pack { } -> throwError "Constructors are not supported yet"
    If { } -> ccExpr mode $ desugarIf expr
    Match{ _expr, _altns } -> do
      ccExpr Stack _expr
      tell [EVAL]
      let MkAltn{ _cons }:_ = _altns
      (_, MkADT{ _constructors }) <- findConstructor _cons
      case _constructors of
        [c] -> ccAltn mode _altns c
        [c0, c1] -> do
          zero <- freshLabel
          done <- freshLabel
          tell [JUMPZERO zero]
          ccAltn mode _altns c1
          tell [JUMP done, LABEL zero]
          ccAltn mode _altns c0
          tell [LABEL done]
        _ -> throwError "Only ADTs with 1 or 2 constructors are supported."

ccAltn :: Mode -> [Altn a] -> Constructor -> CC ()
ccAltn mode altns MkConstructor{ _name } =
  case List.find (\MkAltn{ _cons } -> _cons == _name) altns of
    Nothing -> throwError $ "match statement does not mention " ++ show _name
    Just MkAltn{ _patns, _rhs } -> do
      let arity = length _patns
          (idents, _) = unzipPatns _patns
      tell [UNCONS arity]
      localDecls (reverse idents) (ccExpr mode _rhs)
      whenStack mode $ tell [SLIDE arity]


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
