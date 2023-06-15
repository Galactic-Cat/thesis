# Optimal Algorithm for Geodesic Nearest-point Voronoi Diagrams in Simple Polygons
Paper by Eunjin Oh  
Presentation by Joes de Jonge  
Summary by Simon van Hus (6147879)

---

The geodesic Voronoi diagram partitions an $n$-sided simple polygon into $m$ cells, each of which denote the closest distance to one of $m$ sites in the polygon.
The authors note that the optimal running time for an algorithm that finds this Voronoi diagram is yet unknown, however they do introduce an algorithm that can find it in $O(n+m\log m)$ time.
With this they improve on the previous algorithms which ran in $O(n+m\log m+m\log^2 n)$ and $O(n\log n+m\log m)$ time.

They also prove with their algorithm that the geodesic nearest-point Voronoi diagram can be computed optimally, a question open since 1989.

