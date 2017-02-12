module CoreLang.GMachine.Builtins
  ( uncompiled
  , compiled
  )
  where

import CoreLang.GMachine.GCode
import CoreLang.Language.Syntax

uncompiled :: [Defn ()]
uncompiled =
  [ mk_defn "not" [x]    x false true
  , mk_defn "&&"  [x, y] x y     false
  , mk_defn "||"  [x, y] x true  y
  ]
  where
    x = MkIdent "x"
    y = MkIdent "y"
    false = MkIdent "false"
    true  = MkIdent "true"
    mk_patn _ident = MkPatn { _annot = (), _ident, _type = Nothing }
    mk_var _ident = Var { _annot = (), _ident }
    mk_defn fun args cond_ then_ else_ =
      MkDefn
        { _patn = mk_patn (MkIdent fun)
        , _expr = Lam { _annot = ()
                      , _patns = map mk_patn args
                      , _body  = If { _annot = ()
                                    , _cond  = mk_var cond_
                                    , _then  = mk_var then_
                                    , _else  = mk_var else_
                                    }
                      }
        }

mkGlobal :: String -> Int -> [GInst String] -> Global
mkGlobal name _arity code =
  let _name = Name name
      _code = GLOBSTART _name _arity : map (fmap Name) code
  in  MkGlobal { _name, _arity, _code }

compiled :: [Global]
compiled = concat
  [ neg : binops
  , constructors
  , [if_
    , is_nil, hd, tl
    , fst_, snd_
    , print_, abort
    ]
  ]

neg :: Global
neg = mkGlobal "neg" 1
  [ EVAL
  , NEG
  , UPDATE 1
  , RETURN
  ]

binops :: [Global]
binops =
  [ mk "+"  ADD
  , mk "-"  SUB
  , mk "*"  MUL
  , mk "/"  DIV
  , mk "%"  MOD
  , mk "<"  LES
  , mk "<=" LEQ
  , mk "==" EQV
  , mk "!=" NEQ
  , mk ">=" GEQ
  , mk ">"  GTR
  ]
  where
    mk name inst = mkGlobal name 2
      [ PUSH 1
      , EVAL
      , PUSH 1
      , EVAL
      , inst
      , UPDATE 3
      , POP 2
      , RETURN
      ]

constructors :: [Global]
constructors =
  [ mk "false"   0 (CONS0 0)
  , mk "true"    0 (CONS0 1)
  , mk "nil"     0 (CONS0 0)
  , mk "cons"    2 (CONS2 1)
  , mk "mk_pair" 2 (CONS2 0)
  ]
  where
    mk name arity inst = mkGlobal name arity
      [ inst
      , UPDATE 1
      , RETURN
      ]

if_ :: Global
if_ = mkGlobal "if" 3
  [ PUSH 0
  , EVAL
  , JUMPZERO ".if_false"
  , PUSH 1
  , JUMP ".fi"
  , LABEL ".if_false"
  , PUSH 2
  , LABEL ".fi"
  , EVAL
  , UPDATE 4
  , POP 3
  , UNWIND
  ]

is_nil, hd, tl :: Global
is_nil = mkGlobal "is_nil" 1
  [ EVAL
  , JUMPZERO ".is_nil"
  , CONS0 0
  , JUMP ".lin_si"
  , LABEL ".is_nil"
  , CONS0 1
  , LABEL ".lin_si"
  , UPDATE 1
  , RETURN
  ]
hd = hd_or_tl "hd" HEAD
tl = hd_or_tl "tl" TAIL
hd_or_tl name inst =
  let dot_empty = "." ++ name ++ "_empty"
  in  mkGlobal name 1
      [ EVAL
      , PUSH 0
      , JUMPZERO dot_empty
      , inst
      , EVAL
      , UPDATE 1
      , UNWIND
      , LABEL dot_empty
      , ABORT
      ]

fst_, snd_ :: Global
fst_ = fst_or_snd "fst" HEAD
snd_ = fst_or_snd "snd" TAIL
fst_or_snd name inst = mkGlobal name 1
  [ EVAL
  , inst
  , EVAL
  , UPDATE 1
  , UNWIND
  ]

print_ :: Global
print_ = mkGlobal "print" 2
  [ EVAL
  , PRINT
  , EVAL
  , UPDATE 1
  , UNWIND
  ]

abort :: Global
abort = mkGlobal "abort" 0 [ABORT]
