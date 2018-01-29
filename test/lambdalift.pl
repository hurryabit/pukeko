data Unit =
       | Unit
data Bool =
       | False
       | True
data Pair a b =
       | Pair a b
data Option a =
       | None
       | Some a
data Choice a b =
       | First a
       | Second b
data Dict$Eq a =
       | Dict$Eq (a -> a -> Bool)
data Dict$Ord a =
       | Dict$Ord (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)
data Dict$Monoid m =
       | Dict$Monoid m (m -> m -> m)
data Dict$Ring a =
       | Dict$Ring (a -> a) (a -> a -> a) (a -> a -> a) (a -> a -> a)
data Int
data Char
data Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
data Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
data List a =
       | Nil
       | Cons a (List a)
data Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
data IO a
external print : Int -> IO Unit = "print"
g : Int -> Int -> Int -> Int -> Int =
  fun (a : Int) (b : Int) (c : Int) (d : Int) -> a
main : IO Unit = print (g 1 2 3 4)
