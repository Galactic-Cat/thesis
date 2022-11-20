{-# LANGUAGE InstanceSigs #-}
module RADT (dual,
             Environment (NEmpty), 
             eval, 
             Expression (Add, Let, Mul, Ref, Sin, Val),
             Tape (TEmpty)
    ) where
    data Environment =
        NEmpty |
        NCons Expression Int Environment

    data Expression =
        Add Expression Expression |
        Let Expression Expression |
        Mul Expression Expression |
        Ref Int |
        Sin Expression |
        Val Float

    data Tape =
        TEmpty |
        TCons Float Int Tape

    instance Show Tape where
        show :: Tape -> String
        show TEmpty = "[]" 
        show tss    = show $ gather tss
            where
                gather :: Tape -> [Float]
                gather TEmpty         = []
                gather (TCons v _ ts) = v : gather ts

    eval :: Environment -> Expression -> Float
    eval n               (Add e1 e2) = eval n e1 + eval n e2
    eval NEmpty          (Let e1 e2) = eval (NCons e1 0 NEmpty) e2
    eval n@(NCons _ i _) (Let e1 e2) = eval (NCons e1 (i + 1) n) e2
    eval n               (Mul e1 e2) = eval n e1 * eval n e2
    eval NEmpty          (Ref _)     = error "Reference not in environment"
    eval n               (Ref i)     = eval n (evalRef n)
        where evalRef NEmpty          = error "Reference not in environment"
              evalRef (NCons e ri ns) = if ri == i then e else evalRef ns
    eval n               (Sin e)     = sin $ eval n e
    eval _               (Val v)     = v

    -- buildTape :: Tape -> Expression -> Tape
    -- buildTape TEmpty          (Let _ e2) = buildTape (TCons 0.0 0 TEmpty) e2
    -- buildTape t@(TCons _ i _) (Let _ e2) = buildTape (TCons 0.0 (i + 1) t) e2
    -- buildTape t               _          = t

    dual :: Environment -> Tape -> Expression -> Float -> Tape
    dual n t (Add e1 e2) d = dual n (dual n t e1 d) e2 d
    dual n t (Let e1 e2) d = dual n' t' e2 d
       where n' = case n of
                  NEmpty           -> NCons e1 0       NEmpty
                  nc@(NCons _ i _) -> NCons e1 (i + 1) nc
             t' = case t of
                  TEmpty           -> TCons 0.0 0       TEmpty
                  tc@(TCons _ i _) -> TCons 0.0 (i + 1) tc
    dual n t (Mul e1 e2) d = dual n (dual n t e1 (d * eval n e2)) e2 (d * eval n e1)
    dual n t (Ref ri)    d = dual n (uptape t) (ref n) d
       where uptape TEmpty   = error "tape not long enough"
             uptape (TCons a ti ts)
                 | ri == ti  = TCons (a + d) ti ts
                 | otherwise = TCons a ti (uptape ts)
             ref NEmpty      = error "reference not found in namespace"
             ref (NCons e ni ns)
                 | ri == ni  = e
                 | otherwise = ref ns
    dual n t (Sin e)     d = dual n t e (d * cos (eval n e))
    dual _ t (Val _)     _ = t