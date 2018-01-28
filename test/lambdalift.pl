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
type Dict$Foldable t =
       | Dict$Foldable (∀a b. (a -> b -> b) -> b -> t a -> b) (∀a b. (b -> a -> b) -> b -> t a -> b)
type Dict$Functor f =
       | Dict$Functor (∀a b. (a -> b) -> f a -> f b)
type List a =
       | Nil
       | Cons a (List a)
type Dict$Monad m =
       | Dict$Monad (∀a. a -> m a) (∀a b. m a -> (a -> m b) -> m b)
type IO a
external print : Int -> IO Unit = "print"
g : Int -> Int -> Int -> Int -> Int =
  fun (a : Int) (b : Int) (c : Int) (d : Int) -> a
main : IO Unit = print (g 1 2 3 4)
