# Entanglement Detection with Near-Zero Cost
Paper by Sam Westrick, Jatin Arora, and Umut A. Acar  
Presentation by Maksymilian Demitraszek  
Summary by Simon van Hus (6147879)

---

This paper is about detecting entanglement, a concept from parallel programming.
In parallel programming, entanglement refers to computations accessing concurrently allocated objects.
This can lead to race-conditions and should therefore be avoided.
However, there are no known ways to check if an object is safe, "disentangled", so instead the paper looks at detecting the entanglement instead.

ParallelML is a dialect of the ML programming language that implements so-called fork-join parallelism, in which at any point the main task may split into multiple concurrent tasks, which are all completed before the main task continues with the joined result.
The authors detect entanglement by tagging any data allocation in parallel tasks with the context in which they were allocated.
They do this by referencing a node in the computational graph of the program.
Then, when this value is dereferenced, we check if the tag still points to the same context, if another part of the process wrote to this part of the memory, that means entanglement took place, which is when the program will throw an error.

By virtue of a couple experiments, the authors also show that the overhead of this entanglement detection is low, both regarding time and space complexity.
Depending on the characteristics of the problem experimented with, some show greater improvement with scale, some less.
However, the authors note that overall the scalability is quite good.