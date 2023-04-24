# Data-Parallel Flattening by Expansion
Paper by Martin Elsman, Troels Henriksen, and Niels Serup  
Presentation by Jaan van Gils  
Summary by Simon van Hus (6147879)

---

- GPUs favour regular data (e.g. every row has the same number of columns)
- Useful in array programming
- GPUs work best on arrays
- This paper: Futhark

- Nested arrays (regular (matrix), irregular (ragged))
- Flattening (no wasted space)
    - Irregular flattening overhead:
        - Overhead of storage, of applying segmented operations
        - Becomes difficult to reason or proof about
- Paper is about improving flattening irregular arrays