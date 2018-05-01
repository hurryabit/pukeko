{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module Pukeko.BackEnd.Compiler
  ( compile
  , Program
  )
  where

import Pukeko.Prelude

import           Control.Lens ((+~))
import           Control.Monad.Freer.Fresh
import qualified Data.Map.Extended as Map
import qualified Data.Set as Set

import Pukeko.AST.NoLambda
import Pukeko.BackEnd.Builtins as Builtins
import Pukeko.BackEnd.GCode
import Pukeko.BackEnd.Info

data Context = MkContext
  { _offsets :: Map Name Int
  , _depth   :: Int
  }
makeLenses ''Context

type CC = Eff [Reader Context, Writer [Inst], Fresh, Error Failure]

freshLabel :: CC Name
freshLabel = MkName . ('.' :) . show <$> fresh

output :: [Inst] -> CC ()
output = tell

compile :: Member (Error Failure) effs => Module -> Eff effs Program
compile module_ = either throwError pure $ do
  let MkInfo{_constructors} = info module_
  let constructors =
        map (uncurry Builtins.constructor) (Set.toList _constructors)
  globals <- mapM compileTopDefn module_
  let _globals = constructors ++ globals
  return MkProgram{ _globals, _main = MkName "main" }

compileTopDefn :: TopLevel -> Either Failure Global
compileTopDefn top = case top of
  Def{_name, _binds, _body} -> do
    let n = length _binds
        context = MkContext
          { _offsets = Map.fromList $ zipMaybe _binds [n, n-1 ..]
          , _depth   = n
          }
    ((), code) <- run . runError . evalFresh 0 . runWriter . runReader context $
      ccExpr Redex _body
    let _arity = n
        _code  = GLOBSTART _name _arity : code
    return MkGlobal { _name, _arity, _code }
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
  d <- view depth
  output [UPDATE (d+1), POP d, inst]

ccExpr :: Mode -> Expr -> CC ()
ccExpr mode expr =
  case expr of
    Local{_name} -> do
      MkContext { _offsets, _depth } <- ask
      let offset = _offsets Map.! _name
      output [PUSH (_depth - offset)]
      case mode of
        Stack -> return ()
        Eval  -> output [EVAL]
        Redex -> continueRedex UNWIND
    Global{_name} -> do
      output [PUSHGLOBAL _name]
      case mode of
        Stack -> return ()
        Eval  -> output [EVAL]
        Redex -> continueRedex UNWIND
    External{_name} ->
      ccExpr mode Global{_name = Builtins.externalName _name}
    Pack{ _tag, _arity } -> do
      let _name = Builtins.constructorName _tag _arity
      ccExpr mode Global{_name}
    Num{ _int } -> do
      output [PUSHINT _int]
      whenRedex mode $ continueRedex RETURN
    Ap{ _fun, _args } -> do
      let n = length _args
      let ccExprAt mode i expr = local (depth +~ i) $ ccExpr mode expr
          ccArgs mode = zipWithM_ (ccExprAt mode) [0..] (reverse _args)
      let ccDefault = do
            ccArgs Stack
            ccExprAt Stack n _fun
            output [MKAP n]
            case mode of
              Stack -> return ()
              Eval  -> output [EVAL]
              Redex -> continueRedex UNWIND
      case _fun of
        Pack{_tag, _arity}
          | n > _arity -> impossible  -- type checker catches overapplications
          | n == _arity && _arity > 0 -> do
              ccArgs Stack
              output [CONS _tag _arity]
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
                        output [inst]
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
      zipWithM_ (\rhs k -> local (depth +~ k) $ ccExpr Stack rhs) rhss [0 ..]
      localDecls (map Just idents) $ ccExpr mode _body
      whenStackOrEval mode $ output [SLIDE n]
    Let{ _isrec = True, _defns, _body } -> do
      let n = length _defns
          (idents, rhss) = unzipDefns _defns
      output [ALLOC n]
      localDecls (map Just idents) $ do
        zipWithM_ (\rhs k -> ccExpr Stack rhs >> output [UPDATE (n-k)]) rhss [0 ..]
        ccExpr mode _body
      whenStackOrEval mode $ output [SLIDE n]
    Match{ _expr, _altns } -> do
      ccExpr Eval _expr
      case _altns of
        [altn] -> ccAltn mode altn
        _ -> do
          ls <- mapM (const freshLabel) _altns
          done <- freshLabel
          output [JUMPCASE ls]
          forM_ (zip ls _altns) $ \(l, altn) -> do
            output [LABEL l]
            ccAltn mode altn
            output [JUMP done]
          output [LABEL done]

ccAltn :: Mode -> Altn -> CC ()
ccAltn mode MkAltn{_binds, _rhs}
  | [(idx, bind)] <- filter (isJust . snd) (zip [0..] _binds) = do
      output [PROJ idx]
      localDecls [bind] (ccExpr mode _rhs)
      whenStackOrEval mode $ output [SLIDE 1]
  | otherwise = do
      let arity = length _binds
      output [UNCONS arity]
      localDecls (reverse _binds) (ccExpr mode _rhs)
      whenStackOrEval mode $ output [SLIDE arity]

localDecls :: [Maybe Name] -> CC a -> CC a
localDecls binds cc = do
  let n = length binds
  d <- view depth
  let offs = Map.fromList $ zipMaybe binds [d+1 ..]
  local (offsets %~ Map.union offs) $ local (depth +~ n) cc
