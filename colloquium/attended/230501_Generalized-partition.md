# A New Generalized Partition Crossover for the Traveling Salesman Problem: Tunneling between Local Optima
Paper by Renato Tinós, Darrell Whitley, and Gabriela Ochoa  
Presentation by Christian Grosu  
Summary by Simon van Hus (6147879)

---

This paper introduces a new local search operator for the travelling salesperson problem.
The operator is called the Generalized Partition Crossover 2 (GPX2), and is an iteration on Generalized Partition Crossover (GPX).

The GPX operator can be used to find new local optima in the search space of the travelling salesperson problem.
The main trick here is to combine two solutions that are both locally optimal under a local search operator.
The resulting solution will also be locally optimal under that same operator, while still being a new solution.
This is called "tunneling" between local optima, as in the graph of the fitness function, it would seem that we move through a mountain of local non-optima, to a new local minimum.

After explaining the precise implementation they compare GPX2 to GPX and another recombination operator called IPT.
They compare GPX and GPX2 mostly on the number of found recombinations and the %-successful improvements, on which both GPX2 outperforms GPX.
They compare GPX2 against IPT on a benchmark problem, where GPX2 also seems to outperform IPT, however GPX2 is much less dominating against IPT than against GPX.
When it comes to speed, both IPT and GPX2 are relatively similar.
The authors point out that while the time complexity of GPX2 is $O(n)$, and IPT's $O(n^2)$, the average time of IPT is actually $O(n)$ as it has been well-optimized, explaining why the two operators perform so similar.

<br />
<img 
    src="./memes/travelling salesman.png"
    style="width: 80%; margin-left: 10%;" />