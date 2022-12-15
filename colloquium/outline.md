# Colloquium Outline
Colloquium on the paper "Efficient Dual-Numbers Reverse AD via Well-Known Program Transformations" by Tom Smeding, and Matthijs Vákár.

## Introduction
- Name paper and authors
- Introduce automatic differentiation
- Introduce dual numbers

## Concepts
- Naïve Dual numbers for reverse mode
    - Solution: fairly easy to understand, provably correct, implementable over all language features
    - Problem: exponential time complexity
- Staging function calls to monadic code
    - Solution: no duplicate backpropagator calls
    - Problem: one-hot encoding on inputs, which is inefficient
- Cayley transformation
- Map as a collector
- Mutable arrays
