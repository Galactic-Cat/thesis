A matrix (plural: "matrices")  is a two-dimensional array.

## Shape
The shape of a matrix is given by the number of rows and columns.
This is generally formatted as $r\times c$, where $r$ is the number of rows, and $c$ the number of columns.
This means that the horizontal (rows) dimension is the first dimension, and the columns second.

In [[Accelerate]] matrices are shorthands for two-demensional arrays:
```haskell
type Matrix a = Array (Z :. Int :. Int) a
```
So an array of with a number of row `r` and a number of columns `c` can be constructed as:
```haskell
matrix :: Matrix Int
matrix = fromList (Z :. c :. r) [0..]
```
This is because in Accelerate the innermost dimension (the first dimension) is on the right of the shape expression.