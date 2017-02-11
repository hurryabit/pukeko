{-# LANGUAGE TemplateHaskell #-}
module CoreLang.GMachine.Machine where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State (StateT, runStateT, MonadState (get))
import Data.Array
import Data.Array.IO
import Data.Label.Derive
import Data.Label.Monadic
import Data.Label.Mono (Lens)
import Data.Maybe (catMaybes)

import qualified Data.Map as Map

import CoreLang.GMachine.GCode
import CoreLang.Pretty

type Inst = GInst (Name, Addr)

type Addr = Int

hell :: Addr
hell = 0

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
  deriving (Show, Eq)

data HeapCell = Cell Tag Addr Addr

data GState a = GState
  { _stck :: a Addr StckCell
  , _sptr :: Addr
  , _bptr :: Addr
  , _dump :: [DumpCell]
  , _heap :: a Addr HeapCell
  , _hptr :: Addr
  , _code :: a Addr Inst
  , _cptr :: Addr
  , _tick :: Int
  }
mkLabels [''GState]

type Reg = Lens (->) (GState IOArray) Addr

type Mem a = Lens (->) (GState IOArray) (IOArray Addr a)

newtype GM a = GM { unGM :: ExceptT String (StateT (GState IOArray) IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadIO
           , MonadError String
           , MonadState (GState IOArray)
           )

instance MonadFail GM where
  fail = throwError

execute :: GProg -> Int -> Int -> IO ()
execute (GProg code) stck_size heap_size = do
  case compile code of
    Left error -> putStrLn error
    Right code -> do
      let frst_code = 1
          last_code = frst_code - 1 + length code
          frst_heap = last_code + 1
          last_heap = frst_heap - 1 + heap_size
          frst_stck = last_heap + 1
          last_stck = frst_stck - 1 + stck_size
      _stck <- newArray (frst_stck, last_stck) hell
      let _sptr = frst_stck - 1
          _bptr = frst_stck
          _dump = []
      _heap <- newArray (frst_heap, last_heap) (Cell Nix hell hell)
      let _hptr = frst_heap - 1
      _code <- newListArray (frst_code, last_code) code
      let _cptr = 1
          _tick = 0
          gstate = GState { _stck, _sptr, _bptr, _dump
                          , _heap, _hptr
                          , _code, _cptr
                          , _tick
                          }
      (res, _) <- runStateT (runExceptT (unGM $ cafs >> exec)) gstate
      case res of
        Left error -> putStrLn $ "ERROR: " ++ error
        Right _ -> putStrLn "SUCCESS!"


compile :: MonadError String m => GCode -> m [Inst]
compile code = do
  let target_entry addr inst =
        case inst of
          LABEL     lab   -> Just (lab, addr)
          GLOBSTART lab _ -> Just (lab, addr)
          _               -> Nothing
      target_table = Map.fromList $ catMaybes $ zipWith target_entry [1 ..] code
      lookup_target lab =
        case Map.lookup lab target_table of
          Just addr -> return (lab, addr)
          Nothing   -> throwError $ "UNKNOWN LABEL: " ++ show lab
  mapM (traverse lookup_target) code

cafs :: GM ()
cafs = do
  bnds <- gets code >>= liftIO . getBounds
  forM_ (range bnds) $ \cadr -> do
    inst <- code !@ cadr
    case inst of
      GLOBSTART (name, _) 0 -> do
        hptr =. succ
        hadr <- gets hptr
        (heap, hadr) =@ Cell Fun 0 cadr
        (code, cadr) =@ GLOBSTART (name, hadr) 0
      _ -> return ()

exec :: GM ()
exec = do
  -- gstate <- get
  -- liftIO $ freezeGState gstate >>= putStrLn . prettyShow >> putStr "\n\n"
  inst <- code !# cptr
  cptr =. succ
  tick =. succ
  unless (inst == EXIT) $ do
    -- liftIO $ print inst
    step inst
    exec

step :: Inst -> GM ()
step inst = do
  case inst of
    EVAL   -> eval
    UNWIND -> unwind
    RETURN -> do
      gets bptr >>= (sptr =:) -- empty stack but keep bottom element
      restore                 -- restore bptr and cptr from dump
    JUMP (_, addr) -> cptr =: addr -- jump to offset addr
    JUMPZERO (_, addr) -> do
      Cell (Con t) _ _ <- stck !# sptr >>= (heap !@) -- deref top
      sptr =. pred                                   -- pop
      when (t == 0) $ cptr =: addr                   -- jump to addr
    LABEL _ -> return ()
    PUSH k ->
      gets sptr >>= subtract k >>> (stck !@) -- get top-k
      >>= push                               -- push
    PUSHINT n -> alloc (Cell Int n hell)
    PUSHGLOBAL (_, cadr) -> do
      GLOBSTART (_, hadr) narg <- code !@ cadr -- determine fun arity
      if narg == 0 then                        -- check if handling CAF
        push hadr                              -- if CAF, push addr of its heap cell
      else
        alloc (Cell Fun narg cadr)             -- alloc new fun cell
    GLOBSTART _ narg -> do
      forM_ [1 .. narg] $ \i -> do           -- replace app nodes by args
        Cell App _ addr <-                   -- addr of i-th arg
          gets sptr >>= subtract i           -- calc addr of top-i
          >>> (stck !@) >>= (heap !@)        -- deref top-i
        trgt <- subtract (i-1) <$> gets sptr -- get top-(i-1)
        (stck, trgt) =@ addr                 -- update top-(i-1) addr of i-th arg
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
      adr2 <- stck !# sptr           -- get top-1
      sptr =. pred                   -- pop
      alloc (Cell (Con t) adr1 adr2) -- alloc and push Con cell
    HEAD -> do
      Cell (Con _) addr _ <- stck !# sptr >>= (heap !@) -- deref top
      (stck, sptr) =# addr                                -- replace top by addr
    TAIL -> do
      Cell (Con _) _ addr <- stck !# sptr >>= (heap !@) -- deref top
      (stck, sptr) =# addr                              -- replace top by addr
    NEG -> do
      Cell Int num_ _ <- stck !# sptr >>= (heap !@) -- deref top
      sptr =. pred                                  -- pop
      alloc (Cell Int (negate num_) hell)           -- alloc result & push its addr
    ADD -> binop (+)
    SUB -> binop (-)
    MUL -> binop (*)
    DIV -> binop div
    MOD -> binop mod
    LES -> relop (<)
    LEQ -> relop (<=)
    EQV -> relop (==)
    NEQ -> relop (/=)
    GEQ -> relop (>=)
    GTR -> relop (>)
    PRINT -> do
      Cell Int num_ _ <- stck !# sptr >>= (heap !@) -- get top
      sptr =. pred                                  -- pop
      liftIO (putStrLn $ "OUTPUT: " ++ show num_)
    EXIT -> throwError "STEP EXIT"
    ABORT -> throwError "ABORT"

eval :: GM ()
eval = do
  Cell tag narg addr <- stck !# sptr >>= (heap !@) -- deref top
  case tag of
    App             -> store >> unwind       -- store bptr & cptr to dump & unwind
    Fun | narg == 0 -> store >> cptr =: addr -- store bptr and cptr to dump and
                                             -- jump to instantiation of caf body
        | otherwise -> return ()
    Int             -> return ()
    Con _           -> return ()
    Nix             -> throwError "EVAL NIX"

unwind, unwind' :: GM ()
unwind = do
  Cell tag dat1 dat2 <- stck !# sptr >>= (heap !@) -- deref top
  case tag of
    Int   -> restore -- restore bptr and cptr from dump
    Con _ -> restore -- restore bptr and cptr from dump
    App   -> push dat1 >> unwind -- push addr of fun to apply and continue unwinding
    Fun   -> do
      size <- (-) <$> gets sptr <*> gets bptr  -- calc size of current stack
      if dat1 <= size then do                  -- check if all args to fun are present
        cptr =: dat2                           -- jump to instantiation of fun body
      else do
        gets bptr >>= (sptr =:) -- empty stack but keep bottom element
        restore                 -- restore bptr and cptr from dump
    Nix -> throwError "UNWIND NIX"
unwind' = liftIO (print (UNWIND :: Inst)) >> unwind

binop :: (Int -> Int -> Int) -> GM ()
binop op = do
  Cell Int num1 _ <- stck !# sptr >>= (heap !@) -- deref top
  sptr =. pred                                  -- pop
  Cell Int num2 _ <- stck !# sptr >>= (heap !@) -- deref top-1
  sptr =. pred                                  -- pop
  alloc (Cell Int (num1 `op` num2) hell)        -- alloc result & push its addr

relop :: (Int -> Int -> Bool) -> GM ()
relop op = do
  Cell Int num1 _ <- stck !# sptr >>= (heap !@) -- deref top
  sptr =. pred                                  -- pop
  Cell Int num2 _ <- stck !# sptr >>= (heap !@) -- deref top-1
  sptr =. pred                                  -- pop
  let tag = fromEnum (num1 `op` num2)           -- determine tag
  alloc (Cell (Con tag) hell hell)              -- alloc result & push its addr

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
  gets sptr >>= (bptr =:)

restore :: GM ()
restore = do
  DumpCell { dump_bptr, dump_cptr } <- head <$> gets dump
  bptr =: dump_bptr
  cptr =: dump_cptr
  dump =. tail



-- This is all not very stable
freezeGState :: GState IOArray -> IO (GState Array)
freezeGState gstate@GState{ _stck, _heap, _code } = do
  _stck <- freeze _stck
  _heap <- freeze _heap
  _code <- freeze _code
  return $ gstate { _stck, _heap, _code }


instance Pretty HeapCell where
  pPrint (Cell tag dat1 dat2) =
    let t = case tag of
            Nix   -> "Nix  "
            App   -> "App  "
            Int   -> "Int  "
            Fun   -> "Fun  "
            Con t -> "Con " ++ show t
    in  brackets (text t <+> paddr dat1 <+> paddr dat2)

paddr :: Addr -> Doc
paddr addr =
  let s = show addr
  in  text $ replicate (4 - length s) '0' ++ s

prettyExpr :: GState Array -> Addr -> Doc
prettyExpr gstate@GState{ _heap, _code } addr =
  let Cell tag dat1 dat2 = _heap ! addr
  in  case tag of
        Nix -> error "NIX"
        App -> parens (prettyExpr gstate dat1 <+> prettyExpr gstate dat2)
        Int -> int dat1
        Fun ->
          let GLOBSTART (name, _) _ = _code ! dat2
          in  text (show name)
        Con t ->
          let args = map (prettyExpr gstate) $ takeWhile (hell /=) [dat1, dat2]
          in  hsep $ text "Con" : int t : args

prettyStck :: GState Array -> Doc
prettyStck gstate@GState{ _stck, _sptr, _bptr, _heap } =
  let prettyStckCell addr cell =
        let ptr
              | addr == _sptr && addr == _bptr = "XX ->"
              | addr == _sptr                  = "SP ->"
              | addr == _bptr                  = "BP ->"
              | otherwise                      = "     "
        in  hsep
            [ text ptr
            , paddr addr <> colon
            , paddr cell
            , text "->"
            , pretty (_heap ! cell)
            , braces (prettyExpr gstate cell)
            ]
  in  vcat $ map (uncurry prettyStckCell) $ takeWhile ((_sptr >=) . fst) $ assocs _stck

prettyHeap :: GState Array -> Doc
prettyHeap gstate@GState{ _heap, _hptr } =
  let prettyHeapCell addr cell =
        paddr addr <> colon <+> pretty cell <+> braces (prettyExpr gstate addr)
  in  vcat $ map (uncurry prettyHeapCell) $ takeWhile ((_hptr >=) . fst) $ assocs _heap

prettyDump :: GState Array -> Doc
prettyDump GState{ _dump } =
  let prettyDumpCell DumpCell{ dump_bptr, dump_cptr } =
        paddr dump_bptr <+> paddr dump_cptr
  in  vcat $ map prettyDumpCell (reverse _dump)

prettyCode :: GState Array -> Doc
prettyCode GState{ _code, _cptr } =
  let s = show _cptr
      c = replicate (4-length s) ' ' ++ s
  in  text c <> colon <+> brackets (text $ show $ _code ! _cptr)

prettyTick :: GState Array -> Doc
prettyTick GState{ _tick } =
  let t = show _tick
  in  text $ replicate (4-length t) ' ' ++ t

instance Pretty (GState Array) where
  pPrint gstate =
    vcat
      [ text "STCK" <+> prettyStck gstate
      , text $ replicate 72 '='
      -- , text "HEAP      " <+> prettyHeap gstate
      -- , text $ replicate 72 '='
      , text "DUMP" <+> prettyDump gstate
      , text $ replicate 72 '='
      , text "CODE" <+> prettyCode gstate
      , text "TICK" <+> prettyTick gstate
      ]
