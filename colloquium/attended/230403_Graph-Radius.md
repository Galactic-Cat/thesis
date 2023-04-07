# Augmenting Graphs to Minimize the Radius
Paper by Joachim Gudmundsson, Yuan Sha, and Fan Yao  
Presentation by Jurre Luijten  
Summary by Simon van Hus (6147879)

---

This paper discusses augmenting a non-negative weighted graph $G=(V,E)$ by adding $k$ new edges or shortcuts to $G$ such that the radius is minimized.

The eccentricity of a vertex $v\in V$ is the maximum distance between $v$ and any other vertex in $V$.
The radius is the minimum eccentricity for all vertices in $V$, and the center of a graph $G$ is the vertex with the smallest eccentricity.

The authors propose three algorithms.
First they suggest a 3-approximation algorithm that runs in $O(km+kn\log n)$ time, where $n$ is the number of vertices in the graph, $m$ is the number of edges, and $k$ is the number of shortcuts to add.
They also provide a proof that states there is no polynomial time approximation algorithm better than $(5/3-\epsilon)$-approximation for any $\epsilon>0$ (unless P=NP).

Furthermore, they also provide two exact algorithms that solve the special case where $G$ is a tree.
The first algorithm can do this in $O(n^2\log n)$ time. However, when the graph is embedded in some metric space, dynamic programming can bring this down to $O(\min(k^2n^3,n^4))$ time, but using $O(k^2n^2)$ space.

<br />
<img 
    src="./memes/graph shortcuts.png"
    style="width: 50%; margin-left: 25%;"/>