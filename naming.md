# Handling names and shadowing

## Renamer and AST in general

### Term variables and top level functions

Term variables introduced via local bindings (lambdas, `let`s or patterns in `case` alternatives) are not allowed to shadow each other. However, they are allowed to shadow top level functions. This works because references to local variables use the `EVar` node where references to top level functions use `EVal`. In the future, the latter will also contain the name of the module where the function was defined.

For instance, the following is fine:
```haskell
f :: Int -> Int
f = ...

map :: (a -> b) -> List a -> List b
map f xs = ...
```
In contrast, this shadowing of `xs` is not allowed:
```haskell
map :: (a -> b) -> List a -> List b
map f xs = case xs of
  Nil -> Nil
  Cons x xs -> Cons (f x) (map f xs)
```

Top level functions need to have unique names.


### Type variables

**TODO!**


### Term and type constructors

Term and type constructors live in separate name spaces. Within their individual namespaces they need to have unique names. Term constructors a referenced using `ECon`, `PCon` and `PSimple`. Type constructors are referenced using `TCon`. In ther future, all of these nodes will also contain the name of the module where the constructor was defined.


## Inliner
