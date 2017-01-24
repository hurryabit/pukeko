module CoreLang.Language.Mark4 where

import Control.Monad.Except
import Control.Monad.RWS

import Data.Array (Array, (!), (//))
import qualified Data.Array as Array
import Data.Maybe (mapMaybe)

import CoreLang.Language.Parser (parse)
import CoreLang.Language.Syntax

type Environment = [(Identifier, Addr)]

newtype Addr = Addr Int

instance Show Addr where
  show (Addr n) = 
    let s = show n
        p = if length s < 4 then replicate (4-length s) '0' else ""
    in  '#' : p ++ s

data Primitive = Neg | Add | Sub | Mul | Div
  deriving (Eq, Ord, Show)

builtins :: [(Primitive, (Identifier, Mark4 ()))]
builtins =
  [ (Neg, ("neg", stepUnOp negate))
  , (Add, ("+"  , stepBinOp (+)  ))
  , (Sub, ("-"  , stepBinOp (-)  ))
  , (Mul, ("*"  , stepBinOp (*)  ))
  , (Div, ("/"  , stepBinOp div  ))
  ]

data Node
  = Application Addr Addr
  | Function    Identifier [Identifier] Expr
  | Number      Integer
  | Indirection Addr
  | Primitive   Identifier Primitive
  deriving (Show)

step :: Mark4 ()
step = do
  tick
  dataStack <- isDataStack
  if dataStack then
    undump
  else do
    topAddr <- top
    topNode <- deref topAddr
    case topNode of
      Number _ -> throwError "step: number applied as function"
      Application addr1 addr2 -> do
        node2 <- deref addr2
        case node2 of
          Indirection addr3 -> update topAddr (Application addr1 addr3)
          _                 -> push addr1
      Function _ args body -> do
        bindings <-
          forM args $ \arg -> do
            _ <- pop
            Application _ addr2 <- top >>= deref
            return (arg, addr2)
        root <- pop
        local (bindings ++) (instantiateAndUpdate body root)
        push root
      Indirection addr -> do
        _ <- pop
        push addr
      Primitive _ prim -> do
        let Just (_, primStep) = lookup prim builtins
        primStep

stepUnOp :: (Integer -> Integer) -> Mark4 ()
stepUnOp op = do
  _ <- pop
  appAddr <- top
  Application _ argAddr <- deref appAddr
  argNode <- deref argAddr
  if isDataNode argNode then do
    Number n <- return argNode
    update appAddr (Number $ op n)
  else do
    dump
    push argAddr

stepBinOp :: (Integer -> Integer -> Integer) -> Mark4 ()
stepBinOp op = do
  _ <- pop
  app1Addr <- top
  Application _ arg1Addr <- deref app1Addr
  arg1Node <- deref arg1Addr
  if isDataNode arg1Node then do
    _ <- pop
    app2Addr <- top
    Application _ arg2Addr <- deref app2Addr
    arg2Node <- deref arg2Addr
    if isDataNode arg2Node then do
      Number n1 <- return arg1Node
      Number n2 <- return arg2Node
      update app2Addr (Number $ n1 `op` n2)
    else do
      dump
      push arg2Addr
  else do
    dump
    push arg1Addr

instantiate :: Expr -> Mark4 Addr
instantiate body =
  case body of
    Num n -> alloc (Number n)
    Ap expr1 expr2 -> do
      addr1 <- instantiate expr1
      addr2 <- instantiate expr2
      alloc (Application addr1 addr2)
    Var x -> do
      Just addr <- asks (lookup x)
      return addr
    Let NonRecursive defs expr -> do
      bindings <-
        forM defs $ \(x, rhs) -> do
          addr <- instantiate rhs
          return (x, addr)
      local (bindings ++) (instantiate expr)
    Let Recursive defs expr -> do
      bindings <-
        forM defs $ \(x, _) -> do
          addr <- alloc (Number 0)
          return (x, addr)
      local (bindings ++) $ do
        forM_ (zip defs bindings) $ \((_, rhs), (_, addr)) -> do
          instantiateAndUpdate rhs addr
        instantiate expr
    Constr _ _ -> throwError "instantiate: constructors not implemented"
    Case   _ _ -> throwError "instantiate: case ... of not implemented"
    Lam    _ _ -> throwError "instantiate: lambdas not implemented"

instantiateAndUpdate :: Expr -> Addr -> Mark4 ()
instantiateAndUpdate body target =
  case body of
    Num n -> update target (Number n)
    Ap expr1 expr2 -> do
      addr1 <- instantiate expr1
      addr2 <- instantiate expr2
      update target (Application addr1 addr2)
    Var x -> do
      Just addr <- asks (lookup x)
      update target (Indirection addr)
    Let NonRecursive defs expr -> do
      bindings <-
        forM defs $ \(x, rhs) -> do
          addr <- instantiate rhs
          return (x, addr)
      local (bindings ++) (instantiateAndUpdate expr target)
    Let Recursive defs expr -> do
      bindings <-
        forM defs $ \(x, _) -> do
          addr <- alloc (Number 0)
          return (x, addr)
      local (bindings ++) $ do
        forM_ (zip defs bindings) $ \((_, rhs), (_, addr)) -> do
          instantiateAndUpdate rhs addr
        instantiateAndUpdate expr target
    Constr _ _ -> throwError "instantiateAndUpdate: constructors not implemented"
    Case   _ _ -> throwError "instantiateAndUpdate: case ... of not implemented"
    Lam    _ _ -> throwError "instantiateAndUpdate: lambdas not implemented"

type Stack = [Addr]

data State = State
  { _stack :: Stack
  , _dump  :: [Stack]
  , _heap  :: Array Int (Maybe Node)
  , _free  :: [Int] -- free addresses
  , _ticks :: Int
  }

emptyState :: State
emptyState = State
  { _stack = []
  , _dump  = []
  , _heap  = Array.listArray (1,1) [Nothing]
  , _free  = [1]
  , _ticks = 0
  }

instance Show State where
  show (State { _stack, _dump, _heap, _ticks }) =
    unlines $ concat $
      [ [ "Ticks = " ++ show _ticks
        , "Stack = ["
        ]
      , map showStackItem _stack
      , [ "]"
        , "Dump = ["
        ]
      , map showDumpItem _dump
      , [ "]"
        , "Heap = ["
        ]
      , mapMaybe showHeapItem (Array.assocs _heap)
      , [ "]"
        , replicate 50 '-'
        ]
      ]
    where
      showNode node =
        case node of
          Application addr1 addr2 -> unwords [ "App", show addr1, show addr2 ]
          Function   fun _ _      -> unwords [ "Fun", fun ]
          Number      n           -> unwords [ "Num", show n ]
          Indirection addr        -> unwords [ "Ind", show addr ]
          Primitive   fun _       -> unwords [ "Pri", fun ]
      showStackItem addr@(Addr n) =
        let Just node = _heap ! n
        in  "  " ++ show addr ++ ": " ++ showNode node
      showDumpItem stack = concat
        [ "  ["
        , unwords $ map show stack
        , "]"
        ]
      showHeapItem (_, Nothing)   = Nothing
      showHeapItem (n, Just node) = Just $
        "  " ++ show (Addr n) ++ ": " ++ showNode node

newtype Mark4 a =
  Mark4 { getMark4 :: ExceptT String (RWS Environment String State) a }
  deriving ( Functor, Applicative
           , MonadError String
           , MonadReader Environment
           , MonadWriter String
           , MonadState State
           )

instance Monad Mark4 where
  return  = Mark4 . return
  m >>= f = Mark4 $ getMark4 m >>= getMark4 . f
  fail    = Mark4 . throwError

runMark4 :: Mark4 a -> (Either String a, String)
runMark4 mark1 = evalRWS (runExceptT (getMark4 mark1)) [] emptyState

stackSize :: Mark4 Int
stackSize = gets (length . _stack)

isEmpty :: Mark4 Bool
isEmpty = gets (null . _stack)

top :: Mark4 Addr
top = do
  addr:_ <- gets _stack
  return addr

pop :: Mark4 Addr
pop = do
  addr:_stack <- gets _stack
  modify $ \state -> state { _stack }
  return addr

push :: Addr -> Mark4 ()
push addr = 
  modify $ \state@(State { _stack }) -> state { _stack = addr:_stack }

dump :: Mark4 ()
dump = modify $ \state@(State { _stack, _dump }) ->
  state { _stack = [], _dump = _stack : _dump }

undump :: Mark4 ()
undump = do
  _stack:_dump <- gets _dump
  modify $ \state -> state { _stack, _dump }
    

heapSize :: Mark4 Int
heapSize = gets (Array.rangeSize . Array.bounds . _heap)

doubleHeap :: Mark4 ()
doubleHeap =
  modify $ \state@(State { _heap, _free }) ->
    let (l,h) = Array.bounds _heap
        newHeap = Array.listArray (l,2*h) (Array.elems _heap ++ repeat Nothing)
        newFree = _free ++ [h+1 .. 2*h]
    in  state { _heap = newHeap, _free = newFree }

alloc :: Node -> Mark4 Addr
alloc node = do
  free <- gets _free
  case free of
    [] -> doubleHeap >> alloc node
    addr:_free -> do
      modify $ \state@(State { _heap }) ->
        state
          { _heap = _heap // [(addr, Just node)]
          , _free
          }
      return (Addr addr)

deref :: Addr -> Mark4 Node
deref (Addr addr) = do
  Just node <- gets ((! addr) . _heap)
  return node

update :: Addr -> Node -> Mark4 ()
update (Addr addr) node = do
  modify $ \state@(State { _heap }) ->
    state { _heap = _heap // [(addr, Just node)] }

free :: Addr -> Mark4 ()
free (Addr addr) = do
  modify $ \state@(State { _heap, _free }) ->
    state
      { _heap = _heap // [(addr, Nothing)]
      , _free = addr : _free
      }

tick :: Mark4 ()
tick = modify $ \state@(State { _ticks }) -> state { _ticks = _ticks + 1 }

executeFile :: String -> IO ()
executeFile file = readFile file >>= executeIO

executeIO :: String -> IO ()
executeIO code = do
  let (res, log) = execute code
  putStr log
  putStrLn $ case res of
    Left error -> "error: " ++ error
    Right n    -> "main = " ++ show n

execute :: String -> (Either String Integer, String)
execute code = runMark4 $
  case parse "<interactive>" code of
    Left error -> throwError (show error)
    Right program -> do
      globals <- compile program
      local (globals ++) run
      Number n <- top >>= deref
      return n

compile :: Program -> Mark4 Environment
compile program = do
  functions <-
    forM (program ++ prelude) $ \(fun, args, body) ->
      (,) fun <$> alloc (Function fun args body)
  primitives <-
    forM builtins $ \(prim, (fun, _)) ->
       (,) fun <$> alloc (Primitive fun prim)
  Just addr <- return (lookup "main" functions)
  push addr
  return (functions ++ primitives)

isDataNode :: Node -> Bool
isDataNode node =
  case node of
    Number _ -> True
    _        -> False

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

isDataStack :: Mark4 Bool
isDataStack =
  (&&) <$> (isDataNode <$> (top >>= deref)) <*> gets (isSingleton . _stack)

isFinal :: Mark4 Bool
isFinal = (&&) <$> isDataStack <*> gets (null . _dump)

run :: Mark4 ()
run = do
  over <- isFinal
  get >>= tell . show
  unless over (step >> run)
