# Data-Parallel Flattening by Expansion
Paper by Martin Elsman, Troels Henriksen, and Niels Serup  
Presentation by Jaan Gils  
Summary by Simon van Hus (6147879)

---

As GPUs favour regular data for parallelism, problems producing irregular data structures are often harder to parallelize.
A way to regularize ragged data is by flattening the data into a single dimension, however this also comes with extra overhead.
In this paper, the authors introduce a design pattern they dubbed "flattening-by-expansion" which obtains a full-flattened implementation of specific irregular data-parallel problems.
The authors also implement this pattern as a function in Futhark.

The authors explore three example problems and then introduce the actual code for the ``expand'' function.
They explain how the function can be used on multiple levels in nested data-paralellel problems, even if nested parallelism is not really supported by the language.

Finally, they also show that the performance of their implementation is (asymptotically) as good as other flattening approaches, but they claim it allows the programmer more control.