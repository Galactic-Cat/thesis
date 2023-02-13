{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unused-top-binds #-}

module Rev () where
    import Nos (
        Array (Array),
        ArrayData (Singleton, ArrayData),
        Shape (DIM0, DIMN),
        Index (Z, S),
        Environment (NEmpty, NCons),
        Expression (Add, Apply, Fix, Fold, Generate, Lambda, Let, Lift, Map, Pair, Ref),
        eval)

    import Dag (newGraph, linkNodes, addNode, Graph, Node, getIndex)

    data GExpression where
        GAdd              :: GExpression
        GApply            :: GExpression
        GFix              :: GExpression
        GFold             :: GExpression
        GGenerate         :: GExpression
        ParallelismStart  :: GExpression
        ParallelismTarget :: Shape sh    -> GExpression -> GExpression
        ParallelismEnd    :: GExpression

    (!-) :: Graph a -> Int -> Int
    (!-) g = (-) (length g)

    transform :: Environment env -> Expression env a -> Graph GExpression -> (Graph GExpression, Int)
    transform n (Add   e1 e2)    g = (g4, length g4)
        where
            (g1, e1p) = transform n e1 g                                        -- Add 'e1' to DAG
            (g2, e2p) = transform n e2 g1                                       -- Add 'e2' to DAG
            g3        = addNode g2 GAdd                                         -- Add GAdd to DAG
            g4        = linkNodes (linkNodes g3 (g3 !- e1p) 0) (g3 !- e2p) 0    -- Link nodes 'e1' and 'e2' to GAdd
    transform n (Apply ef e1)    g = (g4, length g4)
        where
            (g1, efp) = transform n ef g                                        -- Add 'ef' to DAG
            (g2, e1p) = transform n e1 g1                                       -- Add 'e1' to DAG
            g3        = addNode g2 GApply                                       -- Add GApply to DAG
            g4        = linkNodes (linkNodes g3 (g3 !- efp) 0) (g3 !- e1p) 0    -- Link nodes 'ef' and 'e1' to GApply
    transform n (Fix   ef)       g = (g3, length g3)
        where
            (g1, efp) = transform n ef g                                        -- Add 'ef' to DAG
            g2        = addNode g1 GFix                                         -- Add GFix to DAG
            g3        = linkNodes g2 (g2 !- efp) 0                              -- Link node 'ef' to GFix
    transform n (Fold  ef ez ea) g = (g5, length g5)
        where
            (g1, efp) = transform n ef g                                        -- Add 'ef' to DAG
            (g2, ezp) = transform n ez g                                        -- Add 'ez' to DAG
            (g3, eap) = transform n ea g                                        -- Add 'ea' to DAG
            (Array sh _) = eval n ea                                            -- Get the shape of Array 'ea'
            (g4, psp) = (addNode g3 ParallelismStart, length g3 + 1)            -- Add ParallelismStart to DAG
            g5        = linkNodes g4 (g4 !- eap) 0                              -- Link 'ea' to ParallelismStart

    foldTransform :: Shape sh -> Graph GExpression -> Int -> Int -> Int -> Graph GExpression
    foldTransform sh g efp ezp psp = DIMN