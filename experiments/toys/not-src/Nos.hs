{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Nos (
    Array (Array),
    ArrayData (Singleton, ArrayData),
    Shape (DIM0, DIMN),
    Index (Z, S),
    Environment (NEmpty, NCons),
    Expression (Add, Apply, Fix, Fold, Generate, Lambda, Let, Lift, Map, Pair, Ref),
    eval
) where
    data Array sh e where
        Array :: Shape sh -> ArrayData e -> Array sh e
    
    data ArrayData e = Singleton e | ArrayData [ArrayData e]

    unwrap :: ArrayData e -> e
    unwrap (Singleton x) = x
    unwrap (ArrayData _) = error "Can't unwrap a non-singleton arraydata"

    data Shape sh where
        DIM0 :: Shape ()
        DIMN :: (Shape sh, Int) -> Shape (sh, Int)

    data Index env a where
        Z :: Index (a ': env) a
        S :: Index env a -> Index (b ': env) a

    data Environment env where
        NEmpty :: Environment '[]
        NCons  :: Environment env -> a -> Environment (a ': env)

    data Expression env a where
        Add      :: (Num a) => Expression env a                       -> Expression env a                  -> Expression env a
        Apply    ::            Expression env (a -> b)                -> Expression env a                  -> Expression env b
        Fix      ::            Expression env (a -> a)                -> Expression env a
        Fold     ::            Expression env (a -> b -> b)           -> Expression env b                  -> Expression env (Array (sh, Int) a) -> Expression env (Array sh b)
        Generate ::            Expression env (Shape sh)              -> Expression env (Shape sh' -> b)   -> Expression env (Array sh b)
        Lambda   ::            (Expression env a -> Expression env b) -> Expression env (a -> b)
        Let      ::            Expression env a                       -> Expression (a ': env) b           -> Expression env b
        Lift     ::            a                                      -> Expression env a
        Map      ::            Expression env (a -> b)                -> Expression env (Array sh a)       -> Expression env (Array sh b)
        Pair     ::            Expression env a                       -> Expression env b                  -> Expression env (a, b)
        Ref      ::            Index env a                            -> Expression env a
        -- TODO: Implement generate, and add shapes

    test :: Expression env Int
    test = 
        Let
            (Lift (3 :: Int))
            (Apply
                (Lambda
                    (Add -- Partial add
                        (Lift (2 :: Int))))
                (Ref Z))

    a = eval NEmpty test

    eval :: Environment env -> Expression env a -> a
    eval n (Add      e1 e2)    = eval n e1 + eval n e2
    eval n (Apply    ef e1)    = eval n ef $ eval n e1
    eval n (Fix      ef)       = eval n ef $ eval n (Fix ef)
    eval n (Fold     ef ez ex) = arrayFold (eval n ef) (eval n ez) (eval n ex)
    eval _ (Generate {})       = error "Not implemented" --arrayGenerate (eval n sh) (eval n gn)
    eval n (Lambda   fn)       = eval n . fn . Lift
    eval _ (Lift     v)        = v
    eval n (Let      er ex)    = eval (NCons n (eval n er)) ex
    eval n (Map      ef ex)    = arrayMap (eval n ef) (eval n ex)
    eval n (Pair     e1 e2)    = (eval n e1, eval n e2)
    eval n (Ref      i)        = envLookup n i

    envLookup :: Environment env -> Index env a -> a
    envLookup NEmpty       _     = error "Environment is empty"
    envLookup (NCons _  e) Z     = e
    envLookup (NCons ns _) (S r) = envLookup ns r

    arrayFold :: (a -> b -> b) -> b -> Array (sh, Int) a -> Array sh b
    arrayFold f z (Array (DIMN (ds, _)) (ArrayData xs)) = Array ds (ArrayData $ foldLoop f z ds xs)
        where
            foldApply :: (a -> b -> b) -> b -> [ArrayData a] -> ArrayData b -- Folds ArrayData of shape "DIMN (DIM0, _)" to "DIM0"
            foldApply _ w []               = Singleton w
            foldApply g w [Singleton y]    = Singleton $ g y w
            foldApply g w (Singleton y:ys) = Singleton $ g y (unwrap (foldApply g w ys))
            foldApply _ _ (ArrayData _:_)  = error "Can't apply fold function on multidimensional array"
            foldLoop :: (a -> b -> b) -> b -> Shape sh -> [ArrayData a] -> [ArrayData b] -- Folds ArrayData of shape "DIMN (DIMN (sh, _), _)" to "DIMN (sh, _)"
            foldLoop _ _   _                          []               = []
            foldLoop g w l@(DIMN (DIMN (DIM0, _), _)) (ArrayData y:ys) = foldApply g w y : foldLoop g w l ys
            foldLoop g w l@(DIMN (ls,   _))           (ArrayData y:ys) = ArrayData (foldLoop g w ls y) : foldLoop g w l ys
            foldLoop _ _   DIM0                       _                = error "Trying to fold over zero-dimensional array"
            foldLoop _ _   (DIMN {})                  (Singleton _:_)  = error "Multidimensional array with zero-dimensional data"
    arrayFold _ _ (Array (DIMN {}) (Singleton _)) = error "Cannot have singletons in multidimensional level of array"

    -- arrayGenerate :: Shape sh -> (Shape sh -> a) -> Array sh a
    -- arrayGenerate DIM0              f = Array DIM0 (Singleton $ f DIM0)
    -- arrayGenerate dss@(DIMN (_, d)) f = Array dss  (ArrayData $ genLoop f dss DIM0 d)
    --     where
    --         genFunc :: (Shape (sh, Int) -> a) -> Shape sh -> Int -> [ArrayData a]
    --         genFunc _ _ 0 = []
    --         genFunc g p i = Singleton (g (DIMN (p, i))) : genFunc g p (i - 1)
    --         genLoop :: (Shape qh -> a) -> Shape sh -> Shape zh -> Int -> [ArrayData a]
    --         genLoop g (DIMN lss@(DIM0, l)) zh i = ArrayData (genFunc g (DIMN (zh, i)) l) : genLoop g lss zh (i - 1)

    arrayMap :: (a -> b) -> Array sh a -> Array sh b
    arrayMap f (Array sh (Singleton v))  = Array sh (Singleton $ f v)
    arrayMap f (Array sh (ArrayData vs)) = Array sh $ ArrayData (mapLoop f vs)
        where
            mapLoop :: (a -> b) -> [ArrayData a] -> [ArrayData b]
            mapLoop _ []               = []
            mapLoop g (Singleton x:xs) = Singleton (g x) : mapLoop g xs
            mapLoop g (ArrayData x:xs) = ArrayData (mapLoop g x) : mapLoop g xs