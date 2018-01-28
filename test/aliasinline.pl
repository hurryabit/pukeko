type Unit =
       | Unit
type Bool =
       | False
       | True
type Pair a b =
       | Pair a b
type Option a =
       | None
       | Some a
type Choice a b =
       | First a
       | Second b
type Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
type Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
type Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
type Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
type Int
type Char
type Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
type Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
type List a =
       | Nil
       | Cons a (List a)
type Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
pure : ∀m. Dict$Monad m -> (∀a. a -> m a) =
  fun @m ->
    fun (dict : Dict$Monad m) ->
      match dict with
      | Dict$Monad @m pure (>>=) -> pure
type IO a
external pure_io : ∀a. a -> IO a = "return"
external bind_io : ∀a b. IO a -> (a -> IO b) -> IO b = "bind"
dict$Monad$IO : Dict$Monad IO =
  let pure : ∀a. a -> IO a = fun @a -> pure_io @a
  and (>>=) : ∀a b. IO a -> (a -> IO b) -> IO b =
        fun @a @b -> bind_io @a @b
  in
  Dict$Monad @IO pure (>>=)
h : ∀c. c -> c = fun @c -> fun (u : c) -> u
main : IO Unit =
  pure @IO dict$Monad$IO @Unit (h @Unit (h @Unit Unit))
