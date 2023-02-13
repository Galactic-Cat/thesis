module Dag (newGraph, addNode, getNode, linkNodes, reverseGraph, Node, Graph, getIndex) where
    type Node a = ([Int], a, [Int])
    type Graph a = [Node a]

    newGraph :: Graph a
    newGraph = []

    addNode :: Graph a -> a -> Graph a
    addNode g v = ([], v, []) : g

    getNode :: Graph a -> Int -> Node a
    getNode = (!!)

    getIndex :: Graph a -> Int -> Int
    getIndex g r = length g + r

    setNode :: Graph a -> Int -> Node a -> Graph a
    setNode g 0 n = n : tail g
    setNode g i n = head g : setNode (tail g) (i - 1) n

    linkNodes :: Graph a -> Int -> Int -> Graph a
    linkNodes g i1 i2 = setNode (setNode g i1 n1) i2 n2
        where
            (o1t, o1, o1f) = getNode g i1
            (o2t, o2, o2f) = getNode g i2
            n1 = (i2 : o1t, o1, o1f)
            n2 = (o2t, o2, i1 : o2f)

    reverseGraph :: Graph a -> Graph a
    reverseGraph [] = []
    reverseGraph ((xt, x, xf):xs) = (xf, x, xt) : reverseGraph xs