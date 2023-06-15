# Compacting Squares: Input-Sensitive In-Place Reconfiguration of Sliding Squares
Paper by Hugo Akitaya, Erik Demaine, Matias Korman, Irina Kostitsyna, Irene Parada, Willem Sonke, Bettina Speckmann, Ryuhei Uehara, and Jules Wulms  
Presentation by Matthijs Wolters  
Summary by Simon van Hus (6147879)

---

This paper is about the sliding squares problem, which is a two-dimensional puzzle about "self-configurable modular robots" that can slide along and around other modules, to form new configurations.
It is proven that any reconfiguration can be done in $O(n^2)$ moves, but also may require $\Omega(n^2)$ moves.
The authors introduce a new algorithm called Gather&Compact, which is a in-place algorithm to solve the reconfiguration problem.

This algorithm is in essence a greedy algorithm, that tries to maximize connectivity among the modules.
This is the "Gather" part of the algorithm, which attempts to gather the leaves from the tree representing all components, into a chunks.
These chunks are then compacted (the "Compact" part) in the second phase.

The algorithm performs in $O(\bar{P}n)$ time, where $\bar{P}$ is the maximum perimeter of the bounding boxes of the two configurations.