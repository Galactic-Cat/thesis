{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Reverse (reverse, Adjoint (AReal, AArray, ASparse)) where
    import Prelude hiding (reverse)
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())
    import Expression (Op1 (Idx, Sin, Sum), Op2 (Add, Mul, Sub))
    import Forward (Forward, Forwarded (FOp1, FOp2, FMap, FMapV, FFold, FFoldV, FJoin), FValue (FArray, FReal))

    data Adjoint
        = AArray  [Float]
        | ANull
        | AReal   Float
        | ASparse Int Float
    
    instance Show Adjoint where
        show :: Adjoint -> String
        show (AArray    as) = "(AArray " ++ show as ++ ")"
        show ANull          = "ANull"
        show (AReal     a)  = "(AReal " ++ show a ++ ")"
        show (ASparse i a)  = "(ASparse " ++ show i ++ " " ++ show a ++ ")"

    type Reverse = Map String ([Adjoint], Adjoint)

    -- Operator to combine two adjoints together
    -- NOTE: The resulting adjoint is shaped like the first argument (not associative)
    (<+) :: Adjoint -> Adjoint -> Adjoint
    (<+) (AArray    as) (AArray    bs) = AArray $ zipWith (+) as bs
    (<+) (AArray    as) (AReal     b)  = AArray $ map (+ b) as
    (<+) (AArray    as) (ASparse i b)  = let bs = drop i as in AArray (take i as ++ (b + head bs) : tail bs)
    (<+) (AReal     a)  (AArray    bs) = AReal $ a + sum bs
    (<+) (AReal     a)  (AReal     b)  = AReal $ a + b
    (<+) (AReal     a)  (ASparse _ b)  = AReal $ a + b
    (<+) (ASparse i a)  (AArray    bs) = let as = drop i bs in AArray (take i bs ++ (a + head as) : tail as)
    (<+) (ASparse _ _)  (AReal     _)  = error "Cannot combine sparse ajdoint with real adjoint, because length of adjoint array is unknown"
    (<+) (ASparse _ _)  (ASparse _ _)  = error "Cannot combine sparse ajdoint with sparse adjoint, because length of adjoint array is unknown"
    (<+) ANull          a              = error $ "Tried to combine null adjoint with " ++ show a
    (<+) a              ANull          = error $ "Tried to combine adjoint " ++ show a ++ " with null adjoint"

    addAdjoint :: String -> Adjoint -> Reverse -> Reverse
    addAdjoint s a r = case Map.lookup s r of
        Just (as, _) -> Map.insert s (a : as, ANull) r
        Nothing      -> Map.insert s ([a], ANull) r

    assignAdjoints :: Forwarded -> String -> Adjoint -> Forward -> Reverse -> (Reverse, [String])
    assignAdjoints (FOp1 op s1) _ a f r = case (op, a) of
        (Idx i, AReal a') -> (addAdjoint s1 (ASparse i a') r, [s1])
        (Sin,   AReal a') -> case getValue s1 f of
            (FReal _ x) -> (addAdjoint s1 (AReal $ a' * cos x) r, [s1])
            _           -> error "Type mismatch in assignAdjoints/FOp1/Sin"
        (Sum,   AReal a') -> case getValue s1 f of
            (FArray _ xs) -> (addAdjoint s1 (AArray $ replicate (length xs) a') r, [s1])
            _             -> error "Type mismatch in assignAdjoints/FOp1/Sum"
        _                 -> error "Type mismatch in assignAdjoints/FOp1"

    assignAdjoints (FOp2 op s1 s2) _ a f r = case (op, a) of
        (Add, _)        -> (addAdjoint s1 a (addAdjoint s2 a r), [s1, s2])
        (Mul, AReal a') -> case (getValue s1 f, getValue s2 f) of
            (FReal _ v1, FReal _ v2) -> (addAdjoint s1 (AReal $ a' * v2) (addAdjoint s2 (AReal $ a' * v1) r), [s1, s2])
            _                        -> error "Type mismatch in assignAdjoints/FOp2/Mul"
        (Sub, _)        -> (addAdjoint s1 a (addAdjoint s2 a r), [s1, s2])
        _               -> error "Type mismatch in assignAdjoints/FOp2"
    
    assignAdjoints (FMap fss s1) s a _ r =
        let as = reverseMap fss 0
            ac = foldl (<+) (AArray $ replicate (length fss) 0.0) as
        in  (addAdjoint s1 ac r, [s1])
        where
            reverseMap :: [Forward] -> Int -> [Adjoint]
            reverseMap []     _ = []
            reverseMap (f:fs) i =
                let s' = s ++ '!' : show i
                    rx = reverse f s' (indexAdjoint i a)
                    ax = toSparse i $ fst $ combineAdjoints (s1 ++ '!' : show i) f rx
                in  ax : reverseMap fs (i + 1)
            toSparse :: Int -> Adjoint -> Adjoint
            toSparse i (AReal a') = ASparse i a'
            toSparse _ _          = error "Type mismatch in assignAdjoints/FMap"

    assignAdjoints (FMapV f' s1) s a f r = error "Couldn't figure out implementation for FMapV"

    assignAdjoints (FFold f s1 s2) s a _ r =
        let r' = reverse f s a
            a' = snd $ r' Map.! s1
        in  case Map.lookup s2 r' of
            Just (_, z') -> (addAdjoint s1 a' (addAdjoint s2 z' r), [s1, s2])
            Nothing      -> (addAdjoint s1 a' r, [s1])

    assignAdjoints (FFoldV f1 q f2 s1 s2) s a _ r =
        if   Map.null f2
        then let r' = reverse f1 s a
                 a' = snd $ r' Map.! s1
                 z' = snd $ r' Map.! s1
             in  (addAdjoint s1 a' (addAdjoint s2 z' r), [s1, s2])
        else let r2 = reverse f2 s a
                 a2 = snd $ r2 Map.! q
                 ss = getJoin
                 (a1s, z') = getParts ss a2
                 a1 = foldl (<+) (AArray $ replicate (Map.size f1) 0.0) a1s
             in  (addAdjoint s1 a1 (addAdjoint s2 z' r), [s1, s2])
        where
            getJoin :: [String]
            getJoin = case f2 Map.! q of
                (FJoin ss, _, _) -> ss
                _                -> error "Type mismatch in assignAdjoints/FFoldV"
            getParts :: [String] -> Adjoint -> ([Adjoint], Adjoint)
            getParts []      _              = ([], AReal 0.0)
            getParts (s':ss) (AArray (a':as)) =
                let r'  = reverse f1 s' (AReal a')
                    a'' = snd $ r' Map.! s'
                    (ra, z') = getParts ss (AArray as)
                in  case Map.lookup s2 r' of
                    Just (_, z'') -> (a'' : ra, z' <+ z'')
                    Nothing       -> (a'' : ra, z')

    assignAdjoints (FJoin  sss) _ a _ r = (assignAll sss a, sss)
        where
            assignAll []     _                = r
            assignAll (s:ss) (AArray (a':as)) = addAdjoint s (AReal a') (assignAll ss (AArray as))

    assignAdjoints _ _ _ _ r = (r, [])

    combineAdjoints :: String -> Forward -> Reverse -> (Adjoint, Reverse)
    combineAdjoints s f r =
        let (as, _) = r Map.! s
            a       = foldr (<+) empty as
        in  (a, Map.insert s (as, a) r)
        where
            empty :: Adjoint
            empty = case getValue s f of
                    (FArray _ xs) -> AArray $ replicate (length xs) 0.0
                    (FReal  {})   -> AReal 0.0
                    _             -> error "Type mismatch in combineAdjoints/empty"
    
    -- Helper function for getting the intermediate value of an element in the forward trace
    getValue :: String -> Forward -> FValue
    getValue s f = let (_, _, v) = f Map.! s in v


    -- Gets the value at a certain index of an array adjoint
    indexAdjoint :: Int -> Adjoint -> Adjoint
    indexAdjoint 0 (AArray (a:_))  = AReal a
    indexAdjoint i (AArray (_:as)) = indexAdjoint (i - 1) (AArray as)
    indexAdjoint _ (AArray [])     = error "Out of range in indexAdjoint"
    indexAdjoint _ _               = error "Type mismatch in indexAdjoint"

    -- Checks if an adjoint has all its parts ready, and combines them if necessary
    resolve :: String -> Forward -> Reverse -> Reverse
    resolve s f r = case Map.lookup s r of
        Just (as, ANull) -> case Map.lookup s f of
            Just (fd, c, _) -> if   length as >= c 
                               then let (a,  r1) = combineAdjoints s f r
                                        (r2, sa) = assignAdjoints fd s a f r1
                                    in  explore sa r2
                               else r
            Nothing         -> error $ "Variable " ++ show s ++ " not in forward trace:\n" ++ show f
        Just _           -> r
        Nothing          -> r
        where
            explore :: [String] -> Reverse -> Reverse
            explore []      r' = r'
            explore (s':ss) r' = explore ss (resolve s' f r')

    -- Wrapper function for the overall reverse pass
    -- Adds the final adjoint to the an empty reverse map so it can be resolved
    reverse :: Forward -> String -> Adjoint -> Reverse
    reverse f s a = resolve s f $ Map.singleton s ([a], ANull)