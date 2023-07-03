# Notes on JAX
Questions to be answered:
- Does JAX use tracing in AD?
- How does JAX allow for repeated differentiation?
    - What is returned by the `grad` functions?

## Transformations
JAX comes with transformation functions, the three most important of which are:
- `grad`, which computes the gradient of a function using automatic differentiation. (Forward mode and reverse mode are also available separately.)
- `jit`, which compiles a function the first time it's run, and then caches it for faster use.
- `vmap`, which vectorizes a function and maps it over an array.

These three transformations are freely composable in arbitrary order with each others or with other transformations in JAX.

## NumPy and lax
JAX also supports a NumPy-like syntax (jNumPy), which in large part mimics the behaviour of NumPy.
However, unlike NumPy, JAX's arrays are immutable.

JAX's jNumPy operations are built on a lower-level API called lax.
lax is stricter than jNumPy, but also more powerful.
For instance, whereas jNumpy will implicitly promote arguments to allow operations between different data types (like `jnp.add(1, 1.0)`), lax requires explicit promotion and would error out on the same operation.
lax also implements more general functions than jNumPy, making it more general.

## JIT compilation
Not all operations can be JIT compiled, JAX requires array shapes to be static and known at compile time.
This is because JIT compilation in JAX is actually just tracing of shapes and dtypes.

# AD process
The AD process follows the following steps:
- Get the forward mode AD as a function implementing a linear map
    - `jvp :: (a -> b) -> (a, T a) -> (b, T b)`
- Instead get the linear mapping part from this using partial execution:
    - `linearize :: (a -> b) -> a -> (b, T a -o T b)`
- Get the reverse mode AD as a function implementing the transpose of that map
    - `transpose :: (a -o b) -> (b -o a)`
- Define `vjp` as:
    - ```haskell
        vjp f (x, ty) =
            let (y, fw) = linearize f x
                rv      = transpose fw
            in  (y, rv ty)
    ```