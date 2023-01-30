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

    -- NOTE: This doesn't work because we don't know how many nodes a subexpression will add :/
    transform :: Environment env -> Expression env a -> Graph GExpression -> Graph GExpression
    transform n (Add   e1 e2)    g = g3
        where
            g1 = addNode (transform n e2 (transform n e1 g)) GAdd                                           -- Add 'e1', 'e2', and GAdd to the DAG
            g2 = linkNodes g1 2 0                                                                           -- Link 'e1' to GAdd
            g3 = linkNodes g2 2 0                                                                           -- Link 'e2' to GAdd
    transform n (Apply ef e1)    g = g3
        where
            g1 = addNode (transform n ef (transform n e1 g)) GApply                                         -- Add 'e1', 'e2', and GApply to the DAG
            g2 = linkNodes g1 2 0                                                                           -- Link 'ef' to GApply
            g3 = linkNodes g2 1 0                                                                           -- Link 'e2' to GApply
    transform n (Fix   ef)       g = g2
        where
            g1 = addNode (transform n ef g) GFix                                                            -- Add 'ef' and GFix to the DAG
            g2 = linkNodes g1 1 0                                                                           -- Link 'ef' to GFix
    transform n (Fold  ef ez e1) g = _
        where
            (Array sh _) = eval n e1
            g1           = addNode (transform n ef (transform n ez (transform n e1 g))) ParallelismStart    -- Add 'ef', 'ez', 'e1', and ParallelismStart to the DAG
            g2           = linkNodes g1 3 0                                                                 -- Link 'e1' to ParallismStart