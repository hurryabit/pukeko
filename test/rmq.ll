external abort : ∀a. a = "abort"
let (&&) : Bool -> Bool -> Bool =
      fun (x : Bool) (y : Bool) ->
        match x with
        | False -> False
        | True -> y
let (||) : Bool -> Bool -> Bool =
      fun (x : Bool) (y : Bool) ->
        match x with
        | False -> y
        | True -> True
external (+) : Int -> Int -> Int = "add"
external (-) : Int -> Int -> Int = "sub"
external (<) : Int -> Int -> Bool = "lt"
external (<=) : Int -> Int -> Bool = "le"
external (>) : Int -> Int -> Bool = "gt"
let zip_with : ∀a b c. (a -> b -> c) -> List a -> List b -> List c =
      fun @a @b @c ->
        fun (f : a -> b -> c) (xs : List a) (ys : List b) ->
          match xs with
          | Nil @a -> Nil @c
          | Cons @a x xs ->
            match ys with
            | Nil @b -> Nil @c
            | Cons @b y ys -> Cons @c (f x y) (zip_with @a @b @c f xs ys)
let replicate : ∀a. Int -> a -> List a =
      fun @a ->
        fun (n : Int) (x : a) ->
          match (<=) n 0 with
          | False -> Cons @a x (replicate @a ((-) n 1) x)
          | True -> Nil @a
external return : ∀a. a -> IO a = "return"
external print : Int -> IO Unit = "print"
external input : IO Int = "input"
external (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
let sequence_io$ll1 : ∀a. a -> List a -> IO (List a) =
      fun @a ->
        fun (x : a) (xs : List a) -> return @(List a) (Cons @a x xs)
let sequence_io$ll2 : ∀a. List (IO a) -> a -> IO (List a) =
      fun @a ->
        fun (ms : List (IO a)) (x : a) ->
          (>>=) @(List a) @(List a) (sequence_io @a ms) (sequence_io$ll1 @a x)
let sequence_io : ∀a. List (IO a) -> IO (List a) =
      fun @a ->
        fun (ms : List (IO a)) ->
          match ms with
          | Nil @(IO a) -> return @(List a) (Nil @a)
          | Cons @(IO a) m ms -> (>>=) @a @(List a) m (sequence_io$ll2 @a ms)
let nats$ll1 : (Int -> List Int) -> Int -> List Int =
      fun (nats_from : Int -> List Int) (n : Int) ->
        Cons @Int n (nats_from ((+) n 1))
let nats : List Int =
      let rec nats_from : Int -> List Int = nats$ll1 nats_from in
      nats_from 0
let pair : ∀a. (a -> a -> a) -> List a -> List a =
      fun @a ->
        fun (op : a -> a -> a) (xs1 : List a) ->
          match xs1 with
          | Nil @a -> Nil @a
          | Cons @a pair$pm1 pair$pm2 ->
            match pair$pm2 with
            | Nil @a -> xs1
            | Cons @a x2 xs3 -> Cons @a (op pair$pm1 x2) (pair @a op xs3)
let single : ∀a. Int -> a -> RmqTree a =
      fun @a ->
        fun (i : Int) (x : a) ->
          RmqNode @a i i x (RmqEmpty @a) (RmqEmpty @a)
let combine : ∀a. (a -> a -> a) -> RmqTree a -> RmqTree a -> RmqTree a =
      fun @a ->
        fun (op : a -> a -> a) (t1 : RmqTree a) (t2 : RmqTree a) ->
          match t1 with
          | RmqEmpty @a -> abort @(RmqTree a)
          | RmqNode @a s1 combine$pm1 v1 combine$pm2 combine$pm3 ->
            match t2 with
            | RmqEmpty @a -> abort @(RmqTree a)
            | RmqNode @a combine$pm4 e2 v2 combine$pm5 combine$pm6 ->
              RmqNode @a s1 e2 (op v1 v2) t1 t2
let build$ll1 : ∀a. (a -> a -> a) -> (List (RmqTree a) -> RmqTree a) -> List (RmqTree a) -> RmqTree a =
      fun @a ->
        fun (op : a -> a -> a) (run : List (RmqTree a) -> RmqTree a) (ts : List (RmqTree a)) ->
          match ts with
          | Nil @(RmqTree a) -> abort @(RmqTree a)
          | Cons @(RmqTree a) build$pm1 build$pm2 ->
            match build$pm2 with
            | Nil @(RmqTree a) -> build$pm1
            | Cons @(RmqTree a) build$pm3 build$pm4 ->
              run (pair @(RmqTree a) (combine @a op) ts)
let build : ∀a. (a -> a -> a) -> List a -> RmqTree a =
      fun @a ->
        fun (op : a -> a -> a) (xs : List a) ->
          let rec run : List (RmqTree a) -> RmqTree a = build$ll1 @a op run
          in
          run (zip_with @Int @a @(RmqTree a) (single @a) nats xs)
let query$ll1 : ∀a. a -> (a -> a -> a) -> Int -> Int -> (RmqTree a -> a) -> RmqTree a -> a =
      fun @a ->
        fun (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) (aux : RmqTree a -> a) (t : RmqTree a) ->
          match t with
          | RmqEmpty @a -> one
          | RmqNode @a t_lo t_hi value left right ->
            match (||) ((<) q_hi t_lo) ((>) q_lo t_hi) with
            | False ->
              match (&&) ((<=) q_lo t_lo) ((<=) t_hi q_hi) with
              | False -> op (aux left) (aux right)
              | True -> value
            | True -> one
let query : ∀a. a -> (a -> a -> a) -> Int -> Int -> RmqTree a -> a =
      fun @a ->
        fun (one : a) (op : a -> a -> a) (q_lo : Int) (q_hi : Int) ->
          let rec aux : RmqTree a -> a = query$ll1 @a one op q_lo q_hi aux in
          aux
let infinity : Int = 1000000000
let min : Int -> Int -> Int =
      fun (x : Int) (y : Int) ->
        match (<=) x y with
        | False -> y
        | True -> x
let replicate_io : ∀a. Int -> IO a -> IO (List a) =
      fun @a ->
        fun (n : Int) (act : IO a) ->
          sequence_io @a (replicate @(IO a) n act)
let main$ll1 : RmqTree Int -> Int -> Int -> IO Unit =
      fun (t : RmqTree Int) (lo : Int) (hi : Int) ->
        let res : Int = query @Int infinity min lo hi t in
        print res
let main$ll2 : RmqTree Int -> Int -> IO Unit =
      fun (t : RmqTree Int) (lo : Int) ->
        (>>=) @Int @Unit input (main$ll1 t lo)
let main$ll3 : List Unit -> IO Unit =
      fun (x : List Unit) -> return @Unit Unit
let main$ll4 : Int -> List Int -> IO Unit =
      fun (m : Int) (xs : List Int) ->
        let t : RmqTree Int = build @Int min xs in
        (>>=) @(List Unit) @Unit (replicate_io @Unit m ((>>=) @Int @Unit input (main$ll2 t))) main$ll3
let main$ll5 : Int -> Int -> IO Unit =
      fun (n : Int) (m : Int) ->
        (>>=) @(List Int) @Unit (replicate_io @Int n input) (main$ll4 m)
let main$ll6 : Int -> IO Unit =
      fun (n : Int) -> (>>=) @Int @Unit input (main$ll5 n)
let main : IO Unit = (>>=) @Int @Unit input main$ll6
