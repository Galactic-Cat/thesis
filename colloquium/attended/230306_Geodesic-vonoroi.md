# Dynamic Geodesic Nearest Neighbour Searching in a Simple Polygon
Paper by Lars Arge and Frank Staals  
Presentation by Lorenzo Theunissen  
Summary by Simon van Hus (6147879)

---

This paper is about performing nearest neighbour search in a simple polygon.
An example of this problem would be tracking the migration of animals or plants, where the search sites would represents current and previous locations of animals or plants, and the polygon would represent any obstacles that would prevent migration.
The authors use a "geodesic Vonoroi diagram", which is a diagram that represents the the geodesic nearest neighbour of a site.

However, the complexity of creating such a Vonoroi diagram for all sites in the entire polygon is $\Omega(nm)$, where $n$ is the number of sites and $m$ is the number of vertices on the polygon.
For large polygons and many sites building such a Vonoroi diagram is fairly slow, so needs to be sped up.
The authors do this by splitting the polygon into multiple sub-polygons, by recursively halving the remaining polygon and storing this in a binary tree.
This binary tree has $O(\log(m))$ layers, and each node of the tree stores a sub-polygon containing a number of sites.
Each of these sites then stores an abstract geodesic Vonoroi diagram for itself.
This then shortens the query time to $O(\log^2n\log^2m)$ time.

<br />
<img
    src="./memes/neighbourhood search.png"
    style="width: 50%; margin-left: 25%;"
    title="Two horses in a confusing perspective to look like one horse"
    alt="Two horses in a confusing perspective to look like one horse" />