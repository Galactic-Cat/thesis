{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lang (Expression (Let, Res), RHS (Add, Sub, Mul, ConsArray, EmptyArray, Fold, Map, Pair, Fst, Snd, Sin, Val, Generate), Idx (Z, S), eval, Environment (NEmpty), Array (AEmpty, ACons, ADim), foldTest) where
    data Expression env a where
        Let :: RHS env a -> Expression (a ': env) b -> Expression env b
        Res :: Idx env a -> Expression env a

    data RHS env a where
        Add        :: (Num a)      => Idx env a             -> Idx env a            -> RHS env a
        CastFloat  ::                                          Idx env Int          -> RHS env Float
        ConsArray  ::                 Idx env (Array a)     -> Idx env a            -> RHS env (Array a)
        EmptyArray ::                                                                  RHS env (Array a)
        Fold       ::                 Idx env (a -> a -> a) -> Idx env a            -> Idx env (Array a) -> RHS env (Array a)
        Fst        ::                                          Idx env (a, a)       -> RHS env a
        Generate   ::                 Idx env [Int]         -> Idx env ([Int] -> b) -> RHS env (Array b)
        Map        ::                 Idx env (a -> b)      -> Idx env (Array a)    -> RHS env (Array b)
        Mul        :: (Num a)      => Idx env a             -> Idx env a            -> RHS env a
        Pair       ::                 Idx env a             -> Idx env a            -> RHS env (a, a)
        Sin        :: (Floating a) =>                          Idx env a            -> RHS env a
        Snd        ::                                          Idx env (a, a)       -> RHS env a
        Sub        :: (Num a)      => Idx env a             -> Idx env a            -> RHS env a
        Val        ::                                          a                    -> RHS env a

    data Idx env a where
        Z :: Idx (a ': env) a
        S :: Idx env a -> Idx (b ': env) a

    data Environment env where
        NEmpty :: Environment '[]
        NCons  :: Environment env -> a -> Environment (a ': env)

    data Array a = AEmpty | ACons (Array a) a | ADim (Array a) (Array a) | ASingle a
        deriving (Show)

    eval :: Environment env -> Expression env a -> a
    eval n (Let rhs lhs) = eval (NCons n (evalRHS n rhs)) lhs
    eval n (Res idx)     = envLookup n idx

    envLookup :: Environment env -> Idx env a -> a
    envLookup NEmpty _            = error "Environment is empty (envLookup)"
    envLookup (NCons _  v) Z      = v
    envLookup (NCons ns _) (S rs) = envLookup ns rs

    evalRHS :: Environment env -> RHS env a -> a
    evalRHS n (Add r1 r2)       = envLookup n r1 + envLookup n r2
    evalRHS n (CastFloat rv)    = fromIntegral $ envLookup n rv
    evalRHS n (ConsArray rs rv) = ACons (envLookup n rs) (envLookup n rv)
    evalRHS _ EmptyArray        = AEmpty
    evalRHS n (Fold rf rz rt)   = deepFold (envLookup n rf) (envLookup n rz) (envLookup n rt)
    evalRHS n (Fst rt)          = fst $ envLookup n rt
    evalRHS n (Generate rds rg) = generate (envLookup n rds) (envLookup n rg)
    evalRHS n (Map rf rl)       = arrayMap (envLookup n rf) (envLookup n rl)
    evalRHS n (Mul r1 r2)       = envLookup n r1 * envLookup n r2
    evalRHS n (Pair r1 r2)      = (envLookup n r1, envLookup n r2)
    evalRHS n (Sin rv)          = sin $ envLookup n rv
    evalRHS n (Snd rt)          = snd $ envLookup n rt
    evalRHS n (Sub r1 r2)       = envLookup n r1 - envLookup n r2
    evalRHS _ (Val v)           = v

    arrayMap :: (a -> b) -> Array a -> Array b
    arrayMap _ AEmpty       = AEmpty
    arrayMap f (ACons xs x) = ACons (arrayMap f xs) (f x)
    arrayMap f (ADim  xs x) = ADim  (arrayMap f xs) (arrayMap f x)
    arrayMap f (ASingle x)  = ASingle (f x)

    generate :: [Int] -> ([Int] -> a) -> Array a
    generate []     _ = AEmpty
    generate (d:ds) g = genloop ds [] d
        where
            genloop _          _ 0 = AEmpty
            genloop []         p i = ACons (genloop []  p (i - 1)) (g (p ++ [i]))
            genloop zss@(z:zs) p i = ADim  (genloop zss p (i - 1)) (genloop zs (p ++ [i]) z)

    deepFold :: (a -> a -> a) -> a -> Array a -> Array a
    deepFold f z (ADim  xs x) = case deepFold f z x of
        ASingle v -> ACons (deepFold f z xs) v
        v         -> ADim  (deepFold f z xs) v
    deepFold f z (ACons xs x) = case xs of
        ACons _ y -> ASingle (f (f x y) (getValue $ deepFold f z xs))
        AEmpty    -> ASingle (f x z)
        _         -> error "Reached impossible state"
    deepFold f z (ASingle v) = ASingle (f v z)
    deepFold _ _ AEmpty      = AEmpty

    getValue :: Array a -> a
    getValue (ASingle v) = v
    getValue (ACons _ v) = v
    getValue (ADim {})   = error "Attempting to get multidimensional value"
    getValue AEmpty      = error "Attempting to get value from empty array"

    foldTest :: Expression env (Array Int)
    foldTest = Let
        (Val [3, 2]) -- Generator size array
        (Let
            (Val (+)) -- Fold function
            (Let
                (Val (const 6)) -- Generator function
                (Let
                    (Generate (S (S Z)) Z)
                    (Let
                        (Val 0) -- Fold identity
                        (Let
                            (Fold (S (S (S Z))) Z (S Z))
                            (Res Z))))))