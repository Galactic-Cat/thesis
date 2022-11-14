[[Haskell]] type system supports Algebraic Data Types (or ADTs).
ADT allow defining complex types out of combining other types.

ADTs come in two general flavours: [[#Sum Types]] and [[#Product Types]], explained below.

## Sum Types
An example of a sum type is the singly linked list as defined below in [[Haskell]]:
```haskell
data List a = Nil
            | Cons a (List a)
```
This type is considered a sum type because