{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Reverse (reverse, Adjoint (AReal, AArray, ASparse), Reverse) where
    import Prelude hiding (reverse)
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())
    import qualified Data.Set as Set
    import Data.Set (Set)
    import Data.Maybe (fromJust)
    import Expression (Op1 (Idx, Sin, Sum), Op2 (Add, Mul, Sub))
    import Forward (Forward, Forwarded (FOp1, FOp2, FMap, FMapV, FFold, FFoldV, FJoin), FValue (FArray, FReal))

    data Adjoint
        = AArray  [Float]
        | AReal   Float
        | ASparse Int Float

    instance Show Adjoint where
        show :: Adjoint -> String
        show (AArray    as) = "(AArray " ++ show as ++ ")"
        show (AReal     a)  = "(AReal " ++ show a ++ ")"
        show (ASparse i a)  = "(ASparse " ++ show i ++ " " ++ show a ++ ")"

    type Reverse = Map String ([Adjoint], Maybe Adjoint)

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
    (<+) (ASparse _ _)  (AReal     _)  = error "Cannot combine sparse adjoint with real adjoint, because length of adjoint array is unknown"
    (<+) (ASparse _ _)  (ASparse _ _)  = error "Cannot combine sparse adjoint with sparse adjoint, because length of adjoint array is unknown"

    addAdjoint :: String -> Adjoint -> Reverse -> Reverse
    addAdjoint s a r = case Map.lookup s r of
        Just (as, _) -> Map.insert s (a : as, Nothing) r
        Nothing      -> case break (=='!') s of
            (array, '!':index) -> case Map.lookup array r of
                Just (as, _)   -> Map.insert array (sparsify a (read index) : as, Nothing) r
                Nothing        -> Map.insert array ([sparsify a (read index)],    Nothing) r
            _                  -> Map.insert s ([a], Nothing) r
        where
            sparsify :: Adjoint -> Int -> Adjoint
            sparsify (AReal a') i = ASparse i a'
            sparsify a'         _ = a'

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
        (Sub, AReal a') -> (addAdjoint s1 a (addAdjoint s2 (AReal $ -a') r), [s1, s2])
        _               -> error "Type mismatch in assignAdjoints/FOp2"

    assignAdjoints (FMap fss s1) s a _ r =
        let (as, r', ss) = reverseMap fss 0
            -- Fold sparse adjoints into single array adjoint
            a' = foldl (<+) (AArray $ replicate (length fss) 0.0) as
        in  (addAdjoint s1 a' r', Set.toList ss)
        where
            reverseMap :: [Forward] -> Int -> ([Adjoint], Reverse, Set String)
            reverseMap []     _ = ([], r, Set.empty)
            reverseMap (f:fs) i =
                let s'  = s ++ '!' : show i
                    rx  = reverse f s' (indexAdjoint i a)
                    -- Extract the array item for 
                    ax  = toSparse i $ fst $ combineAdjoints s' f rx
                    -- Remove items from the original and destination array from this reverse pass
                    -- and add the array items partial adjoint
                    rx' = Map.delete s' (Map.delete (s1 ++ '!' : show i) rx)
                    -- Find the results of the rest of the map
                    (axs, rxs, sxs) = reverseMap fs (i + 1)
                in  (
                    -- Add this sparse partial to the list
                    ax : axs,
                    -- Add rx' to the main reverse pass
                    Map.unionWith unionReverse rxs rx',
                    -- Add relevant keys to the set of ancestors
                    Set.union sxs $ Map.keysSet rx'
                )
            toSparse :: Int -> Adjoint -> Adjoint
            toSparse i (AReal a') = ASparse i a'
            toSparse _ _          = error "Type mismatch in assignAdjoints/FMap/toSparse"

    assignAdjoints (FMapV f' s1) s a f r = error "Not implemented in assignAdjoints/FMapV (see source code for more detail)"
        -- No time left to implement this but the steps are as follows:
        -- Using the trace f' and the intermediate value for s1, calculate the intermediate values for all subtraces
        -- Then using f' and the intermediate values, we can do the reverse-pass, doing each step on all items
        -- We end up with our adjoint array

    assignAdjoints (FFold f s1 s2) s a _ r =
        let rf = reverse f s a
            ma = snd $ rf Map.! s1
            r' = Map.unionWith unionReverse r (Map.delete s1 $ Map.delete s2 rf)
        in  case (Map.lookup s2 rf, ma) of
            (Just (_, Just z'), Just a') -> (addAdjoint s1 a' (addAdjoint s2 z' r'), [s1, s2])
            (Nothing,           Just a') -> (addAdjoint s1 a' r', [s1])
            _                            -> error "Type mismatch in assignAdjoints/FFold"

    assignAdjoints (FFoldV f1 q f2 s1 s2) s a f r =
        if   Map.null f2
        then let r' = reverse f1 s a
                 aa = fromJust $ snd $ r' Map.! s1
                 az = fromJust $ snd $ r' Map.! s2
             in  (addAdjoint s1 aa (addAdjoint s2 az r), [s1, s2])
        else let r2 = reverse f2 s a
                 a2 = snd $ r2 Map.! q
                 ss = getJoin
                 (a1s, z') = getParts ss $ fromJust a2
                 a1 = foldl (<+) (AArray $ replicate adjointLength 0.0) a1s
             in  (addAdjoint s1 a1 (addAdjoint s2 z' r), [s1, s2])
        where
            adjointLength :: Int
            adjointLength =
                let v = getValue s1 f 
                in  case v of 
                    FArray _ xs -> length xs
                    _           -> error "Type mismatch in assignAdjoints/FFoldV/adjointLength"
            getJoin :: [String]
            getJoin = case f2 Map.! q of
                (FJoin ss, _, _) -> ss
                _                -> error "Type mismatch in assignAdjoints/FFoldV (2)"
            getParts :: [String] -> Adjoint -> ([Adjoint], Adjoint)
            getParts []      _                = ([], AReal 0.0)
            getParts (s':ss) (AArray (a':as)) =
                let r'  = reverse f1 s' (AReal a')
                    a'' = fromJust (snd $ r' Map.! s1)
                    (ra, z') = getParts ss (AArray as)
                in  case Map.lookup s2 r' of
                    Just (_, Just z'') -> (a'' : ra, z' <+ z'')
                    _                  -> (a'' : ra, z')

    assignAdjoints (FJoin  sss) _ a _ r = (assignAll sss a, [])
        where
            assignAll []     _                = r
            assignAll (s:ss) (AArray (a':as)) = addAdjoint s (AReal a') (assignAll ss (AArray as))
            assignAll _      _                = error "Type mismatch in assignAdjoints/FJoin"

    assignAdjoints _ _ _ _ r = (r, [])

    combineAdjoints :: String -> Forward -> Reverse -> (Adjoint, Reverse)
    combineAdjoints s f r =
        let (as, _) = r Map.! s
            a       = foldr (<+) empty as
        in  (a, Map.insert s (as, Just a) r)
        where
            empty :: Adjoint
            empty = case getValue s f of
                    (FArray _ xs) -> AArray $ replicate (length xs) 0.0
                    (FReal  {})   -> AReal 0.0
                    _             -> error "Type mismatch in combineAdjoints/empty"

    -- Helper function for getting the intermediate value of an element in the forward trace
    getValue :: String -> Forward -> FValue
    getValue s f = case Map.lookup s f of
        Just (_, _, v) -> v
        Nothing        -> case break (=='!') s of
            (array, '!':index) -> case Map.lookup array f of
                Just (_, _, FArray _ xs) -> FReal s (xs !! read index)
                Nothing                  -> error $ "Entry for " ++ s ++ " not found in forward map"
            _                  -> error $ "Entry for " ++ s ++ " not found in forward map"

    -- Gets the value at a certain index of an array adjoint
    indexAdjoint :: Int -> Adjoint -> Adjoint
    indexAdjoint 0 (AArray (a:_))  = AReal a
    indexAdjoint i (AArray (_:as)) = indexAdjoint (i - 1) (AArray as)
    indexAdjoint _ (AArray [])     = error "Out of range in indexAdjoint"
    indexAdjoint _ _               = error "Type mismatch in indexAdjoint"

    -- Joins two reverse maps together, left-biased
    -- TODO: Actually implement this in assignAdjoints for FMap, FMapV, FFold, and FFoldV
    unionReverse :: ([Adjoint], Maybe Adjoint) -> ([Adjoint], Maybe Adjoint) -> ([Adjoint], Maybe Adjoint)
    unionReverse (asa, aa) (asb, _) = (asa ++ asb, aa)

    -- Checks if an adjoint has all its parts ready, and combines them if necessary
    resolve :: String -> Forward -> Reverse -> Reverse
    resolve s f r = case Map.lookup s r of
        Just (as, Nothing) -> case Map.lookup s f of
            Just (fd, c, _) -> if   length as >= c
                               then let (a,  r1) = combineAdjoints s f r
                                        (r2, sa) = assignAdjoints fd s a f r1
                                    in  explore sa r2
                               else r
            Nothing         -> error $ "Variable " ++ show s ++ " not in forward trace: (2)\n" ++ show f
        Just _             -> r
        Nothing            -> case break (=='!') s of
            (array, '!':_) -> resolve array f r
            _              -> r
        where
            explore :: [String] -> Reverse -> Reverse
            explore []      r' = r'
            explore (s':ss) r' = explore ss (resolve s' f r')

    -- Wrapper function for the overall reverse pass
    -- Adds the final adjoint to the an empty reverse map so it can be resolved
    reverse :: Forward -> String -> Adjoint -> Reverse
    reverse f s a = resolve s f $ Map.singleton s ([a], Nothing)