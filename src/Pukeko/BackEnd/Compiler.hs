module Pukeko.BackEnd.Compiler
  ( compile
  , Program
  )
  where

import Control.Monad.RWS hiding (asks, local)
import Data.Label.Derive
import Data.Label.Monadic
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Pukeko.Error
import Pukeko.BackEnd.Info
import Pukeko.AST.NoLambda
import Pukeko.BackEnd.GCode
import Pukeko.BackEnd.Builtins as Builtins

data Context = MkContext
  { _offsets :: Map Name Int
  , _depth   :: Int
  }
mkLabels [''Context]

newtype CC a = CC { unCC :: RWST Context [Inst] Int (Except String) a }
  deriving ( Functor, Applicative, Monad
           , MonadError String
           , MonadReader Context
           , MonadWriter [Inst]
           )

freshLabel :: CC Name
freshLabel = CC $ state $ \n -> (MkName ('.':show n), n+1)

compile :: MonadError String m => Module -> m Program
compile module_ = do
  let MkInfo{_constructors} = info module_
  let constructors =
        map (uncurry Builtins.constructor) (Set.toList _constructors)
  globals <- mapM compileTopDefn module_
  let _globals = constructors ++ globals
  return MkProgram{ _globals, _main = MkName "main" }

compileTopDefn :: MonadError String m => TopLevel -> m Global
compileTopDefn top = case top of
  Def{_name, _binds, _body} -> do
    let n = length _binds
        context = MkContext
          { _offsets = Map.fromList $ zipMaybe _binds [n, n-1 ..]
          , _depth   = n
          }
    ((), code) <- runExcept (evalRWST (unCC $ ccExpr Redex _body) context 0)
    let _arity = n
        _code  = GLOBSTART _name _arity : code
    return $ MkGlobal { _name, _arity, _code }
  Asm{_name} -> Builtins.findGlobal _name

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

ccExpr :: Mode -> Expr -> CC ()
ccExpr mode expr =
  case expr of
    Local{_name} -> do
      MkContext { _offsets, _depth } <- ask
      case Map.lookup _name _offsets of
        Nothing     -> bugWith "unknown local variable" _name
        Just offset -> tell [PUSH (_depth - offset)]
      case mode of
        Stack -> return ()
        Eval  -> tell [EVAL]
        Redex -> continueRedex UNWIND
    Global{_name} -> do
      tell [PUSHGLOBAL _name]
      case mode of
        Stack -> return ()
        Eval  -> tell [EVAL]
        Redex -> continueRedex UNWIND
    External{_name} ->
      ccExpr mode $ Global{_name = Builtins.externalName _name}
    Pack{ _tag, _arity } -> do
      let _name = Builtins.constructorName _tag _arity
      ccExpr mode $ Global{_name}
    Num{ _int } -> do
      tell [PUSHINT _int]
      whenRedex mode $ continueRedex RETURN
    Ap{ _fun, _args } -> do
      let n = length _args
      let ccExprAt mode i expr = local depth (+i) $ ccExpr mode expr
          ccArgs mode = zipWithM_ (ccExprAt mode) [0..] (reverse _args)
      let ccDefault = do
            ccArgs Stack
            ccExprAt Stack n _fun
            tell [MKAP n]
            case mode of
              Stack -> return ()
              Eval  -> tell [EVAL]
              Redex -> continueRedex UNWIND
      case _fun of
        Pack{_tag, _arity}
          | n > _arity -> bugWith "overapplied constructor" _fun
          | n == _arity && _arity > 0 -> do
              ccArgs Stack
              tell [CONS _tag _arity]
              case mode of
                Stack -> return ()
                Eval  -> return ()
                Redex -> continueRedex RETURN
          | otherwise -> ccDefault
        External{_name} ->
          case Builtins.findInline _name of
            Nothing -> ccDefault
            Just (inst, arity)
              | n == arity -> do
                  let eval continue = do
                        ccArgs Eval
                        tell [inst]
                        continue
                  case mode of
                    Stack -> ccDefault
                    Eval  -> eval $ return ()
                    Redex -> eval $ continueRedex RETURN
              | otherwise -> ccDefault
        _ -> ccDefault
    Let{ _isrec = False, _defns, _body } -> do
      let n = length _defns
          (idents, rhss) = unzipDefns _defns
      zipWithM_ (\rhs k -> local depth (+k) $ ccExpr Stack rhs) rhss [0 ..]
      localDecls (map Just idents) $ ccExpr mode _body
      whenStackOrEval mode $ tell [SLIDE n]
    Let{ _isrec = True, _defns, _body } -> do
      let n = length _defns
          (idents, rhss) = unzipDefns _defns
      tell [ALLOC n]
      localDecls (map Just idents) $ do
        zipWithM_ (\rhs k -> ccExpr Stack rhs >> tell [UPDATE (n-k)]) rhss [0 ..]
        ccExpr mode _body
      whenStackOrEval mode $ tell [SLIDE n]
    Match{ _expr, _altns } -> do
      ccExpr Eval _expr
      case _altns of
        [altn] -> ccAltn mode altn
        _ -> do
          ls <- mapM (\_ -> freshLabel) _altns
          done <- freshLabel
          tell [JUMPCASE ls]
          forM_ (zip ls _altns) $ \(l, altn) -> do
            tell [LABEL l]
            ccAltn mode altn
            tell [JUMP done]
          tell [LABEL done]

ccAltn :: Mode -> Altn -> CC ()
ccAltn mode MkAltn{_binds, _rhs} = do
  let arity = length _binds
  tell [UNCONS arity]
  localDecls (reverse _binds) (ccExpr mode _rhs)
  whenStackOrEval mode $ tell [SLIDE arity]

localDecls :: [Maybe Name] -> CC a -> CC a
localDecls binds cc = do
  let n = length binds
  d <- asks depth
  let offs = Map.fromList $ zipMaybe binds [d+1 ..]
  local offsets (Map.union offs) $ local depth (+n) $ cc
