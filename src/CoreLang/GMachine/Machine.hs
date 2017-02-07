{-# LANGUAGE DeriveFunctor, DeriveTraversable, TemplateHaskell #-}
module CoreLang.TempInst.Machine
  where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State (StateT, runStateT, MonadState)
import Data.Array.IO
import Data.Char (isSpace)
import Data.Label.Derive
import Data.Label.Monadic
import Data.Label.Mono (Lens)
import Data.Map (Map)
import Data.Maybe (catMaybes)

import qualified Data.Map as Map

data GInst lab
  = EVAL
  | UNWIND
  | RETURN
  | EXIT
  | JUMP        lab
  | JUMPNIL     lab
  | LABEL       lab
  | PUSH        Int
  | PUSHINT     Int
  | PUSHGLOBAL  lab
  | GLOBSTART   lab Int
  | POP         Int
  | SLIDE       Int
  | UPDATE      Int
  | ALLOC       Int
  | MKAP
  | CONS0       Int
  | CONS1       Int
  | CONS2       Int
  | HEAD
  | TAIL
  | NEG
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | PRINT
  deriving (Eq, Read, Show, Foldable, Functor, Traversable)

type Inst = GInst Addr

newtype Name = Name String
  deriving (Eq, Ord)

type GCode = [GInst Name]

type Addr = Int

hell :: Addr
hell = -1

type StckCell = Addr

data DumpCell = DumpCell
  { dump_bptr :: Addr
  , dump_cptr :: Addr
  }

data Tag
  = Nix
  | App
  | Int
  | Fun
  | Con Int
  deriving (Eq)

data HeapCell = Cell Tag Addr Addr

data GState = GState
  { _stck :: IOArray Addr StckCell
  , _sptr :: Addr
  , _bptr :: Addr
  , _dump :: [DumpCell]
  , _heap :: IOArray Addr HeapCell
  , _hptr :: Addr
  , _code :: IOArray Addr Inst
  , _cptr :: Addr
  }
mkLabels [''GState]

type Reg = Lens (->) GState Addr

type Mem a = Lens (->) GState (IOArray Addr a)

newtype GM a = GM { unGM :: ExceptT String (StateT GState IO) a }
  deriving ( Functor, Applicative
           , MonadIO
           , MonadError String
           , MonadState GState
           )

instance Monad GM where
  return  = GM . return
  m >>= f = GM $ unGM m >>= unGM . f
  fail    = GM . throwError

execute :: String -> Int -> Int -> IO ()
execute file stack_size heap_size = do
  let read_inst line =
        case reads line of
          [(inst, "")] -> inst
          _ -> error $ "ERROR: Cannot parse " ++ line
  code <- liftM (map read_inst . filter (not . null) . lines) $ readFile file
  case compile code of
    Left error -> putStrLn error
    Right code -> do
      _stck <- newArray (0, stack_size-1) hell
      let _sptr = -1
          _bptr = 0
          _dump = []
      _heap <- newArray (0, heap_size-1) (Cell Nix hell hell)
      let _hptr = -1
      _code <- newListArray (0, length code-1) code
      let _cptr = 0
          gstate = GState { _stck, _sptr, _bptr, _dump, _heap, _hptr, _code, _cptr }
      (res, _) <- runStateT (runExceptT (unGM exec)) gstate
      case res of
        Left error -> putStrLn $ "ERROR: " ++ error
        Right _ -> putStrLn "SUCCESS!"


compile :: MonadError String m => GCode -> m [Inst]
compile code = do
  let target_entry :: Addr -> GInst Name -> Maybe (Name, Addr)
      target_entry addr inst =
        case inst of
          LABEL     lab   -> Just (lab, addr)
          GLOBSTART lab _ -> Just (lab, addr)
          _               -> Nothing
      target_table :: Map Name Addr
      target_table =
        Map.fromList $ catMaybes $ zipWith target_entry [0 ..] code
      lookup_target :: MonadError String m => Name -> m Addr
      lookup_target lab =
        case Map.lookup lab target_table of
          Just addr -> return addr
          Nothing   -> throwError $ "UNKNOWN LABEL: " ++ show lab
  mapM (traverse lookup_target) code

exec :: GM ()
exec = do
  inst <- code !# cptr
  cptr =. succ
  unless (inst == EXIT) (step inst >> exec)

step :: Inst -> GM ()
step inst = do
  case inst of
    EVAL   -> eval
    UNWIND -> unwind
    RETURN -> do
      gets bptr >>= (sptr =:) -- empty stack but keep bottom element
      restore                 -- restore bptr and cptr from dump
    JUMP addr -> cptr =: addr -- jump to offset addr
    JUMPNIL addr -> do
      Cell (Con t) _ _ <- stck !# sptr >>= (heap !@) -- deref top
      sptr =. pred                             -- pop
      when (t == 0) $ cptr =: addr         -- jump to addr
    LABEL _ -> return ()
    PUSH k ->
      gets sptr >>= subtract k >>> (stck !@) -- get top-k
      >>= push                              -- push
    PUSHINT n -> alloc (Cell Int n hell)
    PUSHGLOBAL addr -> do
      GLOBSTART _ narg <- code !@ addr -- determine fun arity
      alloc (Cell Fun narg addr)
    GLOBSTART _ _ -> return ()
    POP k -> sptr =. subtract k -- pop k elements
    SLIDE k -> do
      addr <- stck !# sptr -- get top
      sptr =. subtract k   -- pop k elements
      (stck, sptr) =# addr -- replace top by old top
    UPDATE k -> do
      addr <- gets sptr >>= subtract k >>> (stck !@)   -- get top-k
      stck !# sptr >>= (heap !@) >>= ((heap, addr) =@) -- replace deref top-k by deref top
      sptr =. pred                                     -- pop
    ALLOC k -> replicateM_ k $ alloc (Cell Nix hell hell) -- alloc and push k Nix cells
    MKAP -> do
      adr1 <- stck !# sptr       -- get top
      sptr =. pred               -- pop
      adr2 <- stck !# sptr       -- get top (previously top-1)
      sptr =. pred               -- pop
      alloc (Cell App adr1 adr2) -- alloc and push App cell
    CONS0 t -> alloc (Cell (Con t) hell hell) -- alloc and push Con cell
    CONS1 t -> do
      addr <- stck !# sptr           -- get top
      sptr =. pred                   -- pop
      alloc (Cell (Con t) addr hell) -- alloc and push Con cell
    CONS2 t -> do
      adr1 <- stck !# sptr           -- get top
      sptr =. pred                   -- pop
      adr2 <- stck !# sptr           -- get top (previously top-1)
      sptr =. pred                   -- pop
      alloc (Cell (Con t) adr1 adr2) -- alloc and push Con cell
    HEAD -> do
      Cell (Con _) addr _ <- stck !# sptr >>= (heap !@) -- deref top
      (stck, sptr) =# addr                              -- replace top by addr
    TAIL -> do
      Cell (Con _) _ addr <- stck !# sptr >>= (heap !@) -- deref top
      (stck, sptr) =# addr                              -- replace top by addr
    NEG -> do
      addr <- stck !# sptr                        -- get top
      Cell Int num_ dat_ <- heap !@ addr          -- deref top
      (heap, addr) =@ Cell Int (negate num_) dat_ -- update deref top with negation
    ADD -> arith (+)
    SUB -> arith (-)
    MUL -> arith (*)
    DIV -> arith div
    MOD -> arith mod
    PRINT -> do
      Cell Int num_ _ <- stck !# sptr >>= (heap !@) -- get top
      liftIO (print num_)
    EXIT -> throwError "STEP EXIT"

eval :: GM ()
eval = do
  Cell tag narg addr <- stck !# sptr >>= (heap !@) -- deref top
  case tag of
    App             -> store >> unwind       -- store bptr and cptr to dump and start unwinding
    Fun | narg == 0 -> store >> cptr =: addr -- store bptr and cptr to dump and
                                             -- jump to instantiation of caf body
        | otherwise -> return ()
    Int             -> return ()
    Con _           -> return ()
    Nix             -> throwError "EVAL NIX"

unwind :: GM ()
unwind = do
  Cell tag dat1 dat2 <- stck !# sptr >>= (heap !@) -- deref top
  case tag of
    Int   -> restore -- restore bptr and cptr from dump
    Con _ -> restore -- restore bptr and cptr from dump
    App   -> push dat1 >> unwind -- push addr of fun to apply and continue unwinding
    Fun   -> do
      size <- (-) <$> gets sptr <*> gets bptr  -- calc size of current stack
      if dat1 <= size then do                  -- check if all args to fun are present
        forM_ [1 .. dat1] $ \i -> do           -- replace app nodes by args
          Cell App _ addr <-                   -- addr of i-th arg
            gets sptr >>= subtract i           -- calc addr of top-i
            >>> (stck !@) >>= (heap !@)        -- deref top-i
          trgt <- subtract (i-1) <$> gets sptr -- get top-(i-1)
          (stck, trgt) =@ addr                 -- update top-(i-1) addr of i-th arg
        cptr =: dat2                           -- jump to instantiation of fun body
      else do
        gets bptr >>= (sptr =:) -- empty stack but keep bottom element
        restore                 -- restore bptr and cptr from dump
    Nix   -> throwError "UNWIND NIX"

arith :: (Int -> Int -> Int) -> GM ()
arith op = do
  Cell Int num1 _    <- stck !# sptr >>= (heap !@) -- deref top
  sptr =. pred                                     -- pop
  addr <- stck !# sptr                             -- get top-1
  Cell Int num2 dat2 <- heap !@ addr               -- deref top-1
  (heap, addr) =@ Cell Int (num1 `op` num2) dat2   -- update deref top-1 with result


infix  3 !@, !#, =@, =#
infixl 9 >>>

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f

(!@) :: Mem a -> Addr -> GM a
mem !@ addr = do
  base <- gets mem
  liftIO $ readArray base addr

(!#) :: Mem a -> Reg -> GM a
mem !# reg = gets reg >>= (mem !@)

(=@) :: (Mem a, Addr) -> a -> GM ()
(mem, addr) =@ cell = do
  base <- gets mem
  liftIO $ writeArray base addr cell

(=#) :: (Mem a, Reg) -> a -> GM ()
(mem, reg) =# cell = do
  addr <- gets reg
  (mem, addr) =@ cell

alloc :: HeapCell -> GM ()
alloc cell = do
  -- TODO: Check heap limit
  hptr =. succ
  (heap, hptr) =# cell
  gets hptr >>= push

push :: Addr -> GM ()
push addr = do
  -- TODO: Check stack limit
  sptr =. succ
  (stck, sptr) =# addr

store :: GM ()
store = do
  dump_bptr <- gets bptr
  dump_cptr <- gets cptr
  dump =. (DumpCell { dump_bptr, dump_cptr}:)

restore :: GM ()
restore = do
  DumpCell { dump_bptr, dump_cptr } <- head <$> gets dump
  bptr =: dump_bptr
  cptr =: dump_cptr
  dump =. tail

instance Show Name where
  show (Name name) = name

instance Read Name where
  readsPrec _ input =
    case break isSpace (dropWhile isSpace input) of
      ("", _)      -> []
      (name, rest) -> [(Name name, rest)]
