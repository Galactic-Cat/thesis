# From Louvain to Leiden: guaranteeing well-connected communities
Paper by Vincent Traag, Ludo Waltman, and Nees Jan van Eck  
Presentation by Eloy Reulen  
Summary by Simon van Hus (6147879)

---

This paper is about the Leiden algorithm for finding communities in graphs.
Communities, closely knit groups of nodes in a graph, tend to exist in complex graph structures, like graphs of scientific paper citation.
Finding these communities can help us better understand the nature of the graph, however the problem is NP-hard, so is generally solved with the help of a heuristic.
A major community finding algorithm was the Louvain algorithm, which tried to optimize a measure called modularity.
However, as the authors of this paper point out, the Louvain algorithm could sometimes disjoint communities because a single "bridge" node would be part of another community.

The Leiden algorithm aims to fix that, along with providing improvements to the running time of the algorithm.
The authors do this by modifying the Louvain algorithm, by first starting with replacing the heuristic method (modularity) with the Constant Potts Model (CPM).
The CPM uses a density parameter $\gamma$ as a threshold for the density of communities.
This allows for finetuning and the CPM (generally) prevents the disjointed communities Louvain suffered from.

Furthermore, the authors note that by only checking changed nodes in the local move phase of the iterative algorithm, they can significantly speed up the local move phase compared to the Louvain algorithm.
They also introduce an additional refinement step before the communities are aggregated, where two nodes are merged if they are well-connected within their community both before and after the merge.

The authors find that the algorithm is both quicker and finds better communities than the Louvain algorithm.

<br />
<img
    src="./memes/fast leiden.png"
    style="width: 55%; margin-left: 22.5%;"
    title="Leiden algorithm as Sonic the Hedgehog responding to Louvain Algorithm's existence with 'gotta go fast'"
    alt="Leiden algorithm as Sonic the Hedgehog responding to Louvain Algorithm's existence with 'gotta go fast'" />