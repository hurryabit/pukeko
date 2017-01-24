module CoreLang.Language.Mark5 where

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

builtins :: [(Identifier, Mark5 ())]
builtins =
  [ ("neg", stepArith1 negate)
  , ("+"  , stepArith2 (+))
  , ("-"  , stepArith2 (-))
  , ("*"  , stepArith2 (*))
  , ("/"  , stepArith2 div)
  , ("<"  , stepRel2 (<) )
  , ("<=" , stepRel2 (<=))
  , ("==" , stepRel2 (==))
  , ("!=" , stepRel2 (/=))
  , (">=" , stepRel2 (>=))
  , (">"  , stepRel2 (>) )
  , ("if"    , stepUnbool)
  , ("unpair", stepUnpair)
  , ("unlist", stepUnlist)
  , ("abort" , throwError "user aborted execution")
  , ("print" , stepPrint)
  ]

data Node
  = Application Addr Addr
  | Function    Identifier [Identifier] Expr
  | Number      Integer
  | Indirection Addr
  | Primitive   Identifier (Mark5 ())
  | Constructor Int Int
  | Data        Int [Addr]

step :: Mark5 ()
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
      Data _ _ -> throwError "step: data applied as function"
      Application addr1 addr2 -> do
        node2 <- deref addr2
        case node2 of
          Indirection addr3 -> update topAddr (Application addr1 addr3)
          _                 -> push addr1
      Function _ args body -> do
        bindings <-
          forM args $ \arg -> do
            _ <- pop
            Application _ argAddr <- top >>= deref
            return (arg, argAddr)
        root <- top
        local (bindings ++) (instantiateAndUpdate body root)
      Indirection addr -> do
        _ <- pop
        push addr
      Primitive _ primStep -> primStep
      Constructor t n -> do
        addrs <-
          replicateM n $ do
            _ <- pop
            Application _ addr <- top >>= deref
            return addr
        root <- top
        update root (Data t addrs)

stepArith1 :: (Integer -> Integer) -> Mark5 ()
stepArith1 op = stepPrim1 $ \node ->
  Number <$> op <$> getNumber node

stepPrim1 :: (Node -> Mark5 Node) -> Mark5 ()
stepPrim1 impl = do
  _ <- pop
  appAddr <- top
  Application _ argAddr <- deref appAddr
  argNode <- deref argAddr
  if isDataNode argNode then do
    resNode <- impl argNode
    update appAddr resNode
  else do
    dump
    push argAddr

stepArith2 :: (Integer -> Integer -> Integer) -> Mark5 ()
stepArith2 op = stepPrim2 $ \node1 node2 ->
  Number <$> (op <$> getNumber node1 <*> getNumber node2)

stepRel2 :: (Integer -> Integer -> Bool) -> Mark5 ()
stepRel2 rel = stepPrim2 $ \node1 node2 ->
  mkBool <$> (rel <$> getNumber node1 <*> getNumber node2)

stepPrim2 :: (Node -> Node -> Mark5 Node) -> Mark5 ()
stepPrim2 impl = do
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
      resNode <- impl arg1Node arg2Node
      update app2Addr resNode
    else do
      dump
      push arg2Addr
  else do
    dump
    push arg1Addr

stepUnbool :: Mark5 ()
stepUnbool = do
  _ <- pop
  condAppAddr <- top
  Application _ condAddr <- deref condAppAddr
  condNode <- deref condAddr
  if isDataNode condNode then do
    _ <- pop -- condAppAddr
    thenAppAddr <- pop
    elseAppAddr <- top
    let rootAddr = elseAppAddr
    cond <- getBool condNode
    if cond then do
      Application _ thenAddr <- deref thenAppAddr
      update rootAddr (Indirection thenAddr)
    else do
      Application _ elseAddr <- deref elseAppAddr
      update rootAddr (Indirection elseAddr)
  else do
    dump
    push condAddr

stepUnpair :: Mark5 ()
stepUnpair = do
  _ <- pop
  pairAppAddr <- top
  Application _ pairAddr <- deref pairAppAddr
  pairNode <- deref pairAddr
  if isDataNode pairNode then do
    _ <- pop -- pairAppAddr
    funAppAddr <- top -- rootAddr
    Data 0 [arg1Addr, arg2Addr] <- return pairNode
    Application _ funAddr <- deref funAppAddr
    arg1AppAddr <- alloc (Application funAddr arg1Addr)
    update funAppAddr (Application arg1AppAddr arg2Addr)
  else do
    dump
    push pairAddr

stepUnlist :: Mark5 ()
stepUnlist = do
  _ <- pop
  listAppAddr <- top
  Application _ listAddr <- deref listAppAddr
  listNode <- deref listAddr
  if isDataNode listNode then do
    _ <- pop -- listAppAddr
    baseAppAddr <- pop
    stepAppAddr <- top -- rootAddr
    Data t argAddrs <- return listNode
    case (t, argAddrs) of
      (0, []) -> do
        Application _ baseAddr <- deref baseAppAddr
        update stepAppAddr (Indirection baseAddr)
      (1, [arg1Addr, arg2Addr]) -> do
        Application _ stepAddr <- deref stepAppAddr
        arg1AppAddr <- alloc (Application stepAddr arg1Addr)
        update stepAppAddr (Application arg1AppAddr arg2Addr)
      _ -> throwError "pattern match failed in stepUnlist"
  else do
    dump
    push listAddr

stepPrint :: Mark5 ()
stepPrint = do
  [] <- gets _dump
  _ <- pop
  outAppAddr <- top
  Application _ outAddr <- deref outAppAddr
  outNode <- deref outAddr
  if isDataNode outNode then do
    num <- getNumber outNode
    tell $ mempty { _output = [num] }
    _ <- pop -- outAppAddr
    retAppAddr <- top -- rootAddr
    Application _ retAddr <- deref retAppAddr
    update retAppAddr (Indirection retAddr)
  else do
    dump
    push outAddr

instantiate :: Expr -> Mark5 Addr
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
    Pack t n -> alloc (Constructor t n)
    Case _ _ -> throwError "instantiate: case ... of not implemented"
    Lam  _ _ -> throwError "instantiate: lambdas not implemented"

instantiateAndUpdate :: Expr -> Addr -> Mark5 ()
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
    Pack t n -> update target (Constructor t n)
    Case _ _ -> throwError "instantiateAndUpdate: case ... of not implemented"
    Lam  _ _ -> throwError "instantiateAndUpdate: lambdas not implemented"

type Stack = [Addr]

data State = State
  { _stack :: Stack
  , _dump  :: [Stack]
  , _heap  :: Array Int (Maybe Node)
  , _free  :: [Int] -- free addresses
  , _ticks :: Int
  }

emptyState :: State
emptyState = 
  let _heap = Array.listArray (1,128) (repeat Nothing)
  in  State
        { _stack = []
        , _dump  = []
        , _heap
        , _free  = Array.indices _heap
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
          Function    fun _ _     -> unwords [ "Fun", fun ]
          Number      n           -> unwords [ "Num", show n ]
          Indirection addr        -> unwords [ "Ind", show addr ]
          Primitive   fun _       -> unwords [ "Pri", fun ]
          Constructor t n         -> unwords [ "Con", show t, show n ]
          Data        t addrs     -> unwords $ "Dat" : show t : map show addrs
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

data Output = Output
  { _output :: [Integer]
  , _log    :: [State]
  }

instance Monoid Output where
  mempty = Output { _output = [], _log = [] }
  o1 `mappend` o2 =
    Output
      { _output = _output o1 <> _output o2
      , _log    = _log    o1 <> _log    o2
      }
  
newtype Mark5 a =
  Mark5 { getMark5 :: ExceptT String (RWS Environment Output State) a }
  deriving ( Functor, Applicative
           , MonadError String
           , MonadReader Environment
           , MonadWriter Output
           , MonadState State
           )

instance Monad Mark5 where
  return  = Mark5 . return
  m >>= f = Mark5 $ getMark5 m >>= getMark5 . f
  fail    = Mark5 . throwError

runMark5 :: Mark5 a -> (Either String a, Output)
runMark5 mark1 = evalRWS (runExceptT (getMark5 mark1)) mempty emptyState

stackSize :: Mark5 Int
stackSize = gets (length . _stack)

isEmpty :: Mark5 Bool
isEmpty = gets (null . _stack)

top :: Mark5 Addr
top = do
  addr:_ <- gets _stack
  return addr

pop :: Mark5 Addr
pop = do
  addr:_stack <- gets _stack
  modify $ \state -> state { _stack }
  return addr

push :: Addr -> Mark5 ()
push addr = 
  modify $ \state@(State { _stack }) -> state { _stack = addr:_stack }

dump :: Mark5 ()
dump = modify $ \state@(State { _stack, _dump }) ->
  state { _stack = [], _dump = _stack : _dump }

undump :: Mark5 ()
undump = do
  _stack:_dump <- gets _dump
  modify $ \state -> state { _stack, _dump }
    

heapSize :: Mark5 Int
heapSize = gets (Array.rangeSize . Array.bounds . _heap)

doubleHeap :: Mark5 ()
doubleHeap =
  modify $ \state@(State { _heap, _free }) ->
    let (l,h) = Array.bounds _heap
        newHeap = Array.listArray (l,2*h) (Array.elems _heap ++ repeat Nothing)
        newFree = _free ++ [h+1 .. 2*h]
    in  state { _heap = newHeap, _free = newFree }

alloc :: Node -> Mark5 Addr
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

deref :: Addr -> Mark5 Node
deref (Addr addr) = do
  Just node <- gets ((! addr) . _heap)
  return node

update :: Addr -> Node -> Mark5 ()
update (Addr addr) node = do
  modify $ \state@(State { _heap }) ->
    state { _heap = _heap // [(addr, Just node)] }

free :: Addr -> Mark5 ()
free (Addr addr) = do
  modify $ \state@(State { _heap, _free }) ->
    state
      { _heap = _heap // [(addr, Nothing)]
      , _free = addr : _free
      }

tick :: Mark5 ()
tick = modify $ \state@(State { _ticks }) -> state { _ticks = _ticks + 1 }

printOutput output =
  putStr $ unlines $
    [ "Output = [" ] ++ map (\n -> "  " ++ show n) output ++ [ "]" ]

executeFile :: String -> IO ()
executeFile file = do
  let logfile = takeWhile (/= '.') file ++ ".log"
  code <- readFile file
  let (res, Output { _output, _log }) = execute code
  printOutput _output
  case res of
    Left error -> do
      putStrLn $ "Error = " ++ error
      writeFile logfile (concatMap show _log)
    Right n    -> do
      putStrLn $ "Ticks  = " ++ show (length _log - 1)
      putStrLn $ "Result = " ++ show n
      writeFile logfile (show (last _log))

executeIO :: String -> IO ()
executeIO code = do
  let (res, Output { _output, _log }) = execute code
  case res of
    Left error -> do
      putStr (concatMap show _log)
      printOutput _output
      putStrLn $ "Error = " ++ error
    Right n -> do
      printOutput _output
      putStrLn $ "Ticks  = " ++ show (length _log - 1)
      putStrLn $ "Result = " ++ show n

execute :: String -> (Either String Integer, Output)
execute code = runMark5 $
  case parse "<interactive>" code of
    Left error -> throwError (show error)
    Right program -> do
      globals <- compile program
      local (globals ++) run
      Number n <- top >>= deref
      return n

compile :: Program -> Mark5 Environment
compile program = do
  functions <-
    forM (program ++ prelude) $ \(fun, args, body) ->
      (,) fun <$> alloc (Function fun args body)
  primitives <-
    forM builtins $ \(fun, prim) ->
       (,) fun <$> alloc (Primitive fun prim)
  Just addr <- return (lookup "main" functions)
  push addr
  return (functions ++ primitives)

isDataNode :: Node -> Bool
isDataNode node =
  case node of
    Number _ -> True
    Data _ _ -> True
    _        -> False

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

isDataStack :: Mark5 Bool
isDataStack =
  (&&) <$> (isDataNode <$> (top >>= deref)) <*> gets (isSingleton . _stack)

isFinal :: Mark5 Bool
isFinal = (&&) <$> isDataStack <*> gets (null . _dump)

run :: Mark5 ()
run = do
  over <- isFinal
  state <- get
  tell $ mempty { _log = [state] }
  unless over (step >> run)

getNumber :: Node -> Mark5 Integer
getNumber node = do
  Number num <- return node
  return num

mkBool :: Bool -> Node
mkBool bool = Data (fromEnum bool) []
  
getBool :: Node -> Mark5 Bool
getBool node = do
  Data t [] <- return node
  return $ toEnum t
