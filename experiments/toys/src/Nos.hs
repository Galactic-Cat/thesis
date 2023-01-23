{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Nos () where
    -- data Array a where
    --     Array :: Index Float Float -> ArrayData a -> Array a

    data ArrayData a = Arr [a] | Ard (ArrayData a)

    data Index env a where
        Z :: Index (a ': env) a
        S :: Index env a -> Index (b ': env) a

    data Environment env where
        NEmpty :: Environment '[]
        NCons  :: Environment env -> a -> Environment (a ': env)

    data Expression env a where
        Add    :: (Num a) => Expression env a                       -> Expression env a        -> Expression env a
        Apply  ::            Expression env (a -> b)                -> Expression env a        -> Expression env b
        Fix    ::            Expression env (a -> a)                -> Expression env a
        Lambda ::            (Expression env a -> Expression env b) -> Expression env (a -> b)
        Lazy   ::            Expression env a                       -> Expression env (Expression env a)
        Let    ::            Expression env a                       -> Expression (Expression env a ': env) b -> Expression env b
        Lift   ::            a                                      -> Expression env a
        Pair   ::            Expression env a                       -> Expression env b        -> Expression env (a, b)
        Ref    ::            Index env a                            -> Expression env a

    test :: Expression env Int
    test = 
        Let
            (Lift (3 :: Int))
            (Apply
                (Lambda
                    (Add
                        (Lift (2 :: Int))))
                        -- Lambda hole
                (Ref Z))

    eval :: Environment env -> Expression env a -> a
    eval n (Add    e1 e2) = eval n e1 + eval n e2
    eval n (Apply  ef e1) = eval n ef $ eval n e1
    eval n (Fix    ef)    = eval n ef $ eval n (Fix ef)
    eval n (Lambda fn)    = eval n . fn . Lift
    eval _ (Lift   v)     = v
    eval n (Pair   e1 e2) = (eval n e1, eval n e2)
    eval n (Ref    i)     = eval n (envLookup n i)

    envLookup :: Environment env -> Index env a -> a
    envLookup NEmpty       _     = error "Environment is empty"
    envLookup (NCons _  e) Z     = e
    envLookup (NCons ns _) (S r) = envLookup ns r
