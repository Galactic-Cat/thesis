{-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns #-}

module Reverse (reverse) where
    import Prelude hiding (reverse)    
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())
    import Data.List (sort)

    import Expression (
        Op1 (Idx, Sin, Sum),
        Op2 (Add, Mul, Sub))

    import Trace (
        TValue (TArray, TReal))

    import Forward (
        addSuccessor,
        Forward,
        Forwarded (FLift, FOp0, FOp1, FOp2, FMap, FMapV))

    import qualified Debug.Trace as Debug (trace)

    -- Partial derivatives from arrays, or real numbers, or missing
    data Reversed = RArray [Float] | RReal Float | RNotC
        deriving (Show)
    -- Stores for each step seeding values and its partial derivate (can be null)
    type Reverse = Map String ([Reversed], Reversed)

    -- Outer wrapper for tracing reverse AD on some real output
    reverse :: Forward -> String -> Float -> Reverse
    reverse f s o = Debug.trace s $
        let r = Map.singleton s ([RReal o], RReal o)
            f' = addSuccessor f s s -- Add final successor to itself so it can seed itself
        in  reverse' f' [s] r

    reverse' :: Forward -> [String] -> Reverse -> Reverse
    reverse' _ []     r = r
    reverse' f (s:ss) r = Debug.trace ("reverse' " ++ s) $
        case getSeed f r s of
            Just o -> Debug.trace ("getSeed " ++ s ++ " = " ++ show o) $
                let (fd, _, _) = f Map.! s
                    (r', a)    = case fd of 
                        FMap  _ _ -> seedMap f s r o
                        FMapV _ _ -> seedMap f s r o
                        _         -> Debug.trace ("seedAncestors " ++ s) $ seedAncestors f (setSeed r s o) o fd
                in  reverse' f (ss ++ a) r'
            Nothing -> reverse' f ss r -- skip this occurrence

    reverse'' :: Forward -> [String] -> Reverse -> [String] -> (Reverse, [String])
    reverse'' _ []     r ks = (r, ks)
    reverse'' f (s:ss) r ks =
        case getSeed f r s of
            Just o ->
                let (fd, _, _) = f Map.! s
                    (r', a)    = case fd of 
                        FMap  _ _ -> seedMap f s r o
                        FMapV _ _ -> seedMap f s r o
                        _         -> seedAncestors f (setSeed r s o) o fd
                in  reverse'' f (ss ++ a) r' ks
            Nothing -> reverse'' f ss r (s:ks)

    getSeed :: Forward -> Reverse -> String -> Maybe Reversed
    getSeed f r s =
        let (t, dss, _) = f Map.! s -- Get successors
        in  Debug.trace ("successors " ++ s ++ " = " ++ show dss) $ case t of
                FMap  _ _ -> arraySeed dss
                FMapV _ _ -> arraySeed dss
                _         -> realSeed dss
        where
            arraySeed :: [String] -> Maybe Reversed
            arraySeed []     = Just $ RArray []
            arraySeed (d:ds) = case r Map.! d of
                (_, RNotC)     -> Nothing
                (_, RArray vs) -> case arraySeed ds of
                    Just (RArray vs') -> Just $ RArray (zipWith (+) vs vs')
                    Just _            -> error "Type mismatch in getSeed/arraySeed/RArray"
                    Nothing           -> Nothing
                (_, RReal v)   -> case arraySeed ds of
                    Just (RArray vs') -> case f Map.! d of
                        (FOp1 (Idx i) _, _, _) -> Just $ RArray (zipWith (+) (sih v i (length vs')) vs')
                        (FOp1 Sum     _, _, _) -> Just $ RArray (zipWith (+) (copy v (length vs')) vs')
                        _                      -> error "Type mismatch in getSeed/arraySeed/RReal (2)"
                    Just _            -> error "Type mismatch in getSeed/arraySeed/RReal (1)"
                    Nothing           -> Nothing
            realSeed :: [String] -> Maybe Reversed
            realSeed []     = Just $ RReal 0
            realSeed (d:ds) = case r Map.! d of
                (_, RNotC)     -> Nothing
                (_, RArray vs) -> case realSeed ds of
                    Just (RReal v') -> Just $ RReal (sum vs + v')
                    Just _          -> error "Type mismatch in getSeed/realSeed/RArray"
                    Nothing         -> Nothing
                (_, RReal v)   -> case realSeed ds of
                    Just (RReal v') -> Just $ RReal (v + v')
                    Just _          -> error "Type mismatch in getSeed/realSeed/RReal"
                    Nothing         -> Nothing

    seedAncestors :: Forward -> Reverse -> Reversed -> Forwarded -> (Reverse, [String])
    seedAncestors _ r _ (FLift _)       = (r, [])
    seedAncestors _ r _ (FOp0  _)       = (r, [])
    seedAncestors f r o (FOp1  op s1)   =
        let (_, _, v1) = f Map.! s1
        in  case (op, v1, o) of
                (Sin,   TReal  _ v, RReal  rv) -> (addSeed r s1 (RReal $ rv * cos v), [s1])
                (Sum,   TArray _ _, RArray rv) -> (addSeed r s1 (RArray rv), [s1])
                (Idx i, TArray _ _, RArray rv) -> (addSeed r s1 (RArray $ ohe i rv), [s1])
                _                              -> error "Type mismatch in seedAncestors/FOp1"
    seedAncestors f r o (FOp2 op s1 s2) =
        let (_, _, v1) = f Map.! s1
            (_, _, v2) = f Map.! s2
        in  case (op, v1, v2, o) of
                (Add, TReal _ _,  TReal _ _,  RReal rv) ->
                    let r1 = Debug.trace ("addSeed " ++ s1 ++ " = " ++ show o) $ addSeed r s1 (RReal rv)
                    in  Debug.trace ("addSeed " ++ s2 ++ " = " ++ show o) (addSeed r1 s2 (RReal rv), [s1, s2])
                (Mul, TReal _ va, TReal _ vb, RReal rv) ->
                    let r1 = addSeed r s1 (RReal $ rv * vb)
                    in  (addSeed r1 s2 (RReal $ rv * va), [s1, s2])
                (Sub, TReal _ _,  TReal _ _,  RReal rv) ->
                    let r1 = addSeed r s1 (RReal rv)
                    in  (addSeed r1 s2 (RReal rv), [s1, s2])
                _                                       -> error "Type mismatch in seedAncestors/FOp2"

    seedMap :: Forward -> String -> Reverse -> Reversed -> (Reverse, [String])
    seedMap f s2 r o =
        let (t, _, _) = f Map.! s2
        in  case (t, o) of
            (FMap tss s1, RArray o') ->
                let (resr, ress) = q tss o' 0 s1
                    ress' = removeDupes ress
                in  (addSeed r s1 resr, ress')
        where
            q :: [Forward] -> [Float] -> Int -> String -> (Reversed, [String])
            q (g:gs) (p:ps) i s1 =
                let so = s1 ++ '!' : show i
                    st = s2 ++ '!' : show i
                    r' = Map.insert st ([], RReal p) r
                    (lr, ls) = reverse'' g [st] r' []
                    (rr, rs) = q gs ps (i + 1) s1
                in  case (rr, snd $ lr Map.! so) of
                    (RArray vs, RReal v) -> (RArray $ zipWithAlt (+) vs (sih v i (i + 1)), ls ++ rs)
    
    addSeed :: Reverse -> String -> Reversed -> Reverse
    addSeed r s1 v = Debug.trace ("addSeed " ++ s1) $ case indexCheck s1 of
        Left  _       -> case Map.lookup s1 r of
            Just (vs, o) -> Map.insert s1 (v : vs, o) r
            Nothing      -> Map.insert s1 ([v], RNotC) r
        Right (s', i) -> case (Map.lookup s' r, v) of
            (Just (vs, o), RReal  v') -> Map.insert s' (RArray (sih v' i (i + 1)) : vs, o) r
            (Just (vs, o), RArray _)  -> Map.insert s' (v : vs, o) r
            (Nothing,      RReal  v') -> Map.insert s' ([RArray $ sih v' i (i + 1)], RNotC) r
            (Nothing,      RArray _)  -> Map.insert s' ([v], RNotC) r

    removeDupes :: (Ord a) => [a] -> [a]
    removeDupes xss = remove $ sort xss
        where
            remove []       = []
            remove [x]      = [x]
            remove (x:y:xs)
                | x == y    = remove (x:xs)
                | otherwise = x : remove (y:xs)

    indexCheck :: String -> Either String (String, Int)
    indexCheck []       = Left []
    indexCheck ('!':ss) = Right ([], read ss)
    indexCheck (s  :ss) = case indexCheck ss of
        Left  s'      -> Left $ s : s'
        Right (s', i) -> Right (s : s', i)

    ohe :: Int -> [Float] -> [Float]
    ohe _ []       = []
    ohe 0 (x:xs) = x : ohe (-1)    xs
    ohe t (_:xs) = 0 : ohe (t - 1) xs

    sih :: Float -> Int -> Int -> [Float]
    sih _ _ 0 = []
    sih v 0 l = v : sih v (-1)    (l - 1)
    sih v i l = 0 : sih v (i - 1) (l - 1)

    copy :: Float -> Int -> [Float]
    copy _ 0 = []
    copy v i = v : copy v (i - 1)

    setSeed :: Reverse -> String -> Reversed -> Reverse
    setSeed r s o = Debug.trace ("setSeed " ++ s) $ case Map.lookup s r of
        Just (rs, _) -> Map.insert s (rs, o) r
        Nothing      -> Map.insert s ([], o) r

    zipWithAlt :: (a -> a -> a) -> [a] -> [a] -> [a]
    zipWithAlt f a b =
        let c = zipWith f a b
        in  if   length a > length b
            then c ++ drop (length c) a
            else c ++ drop (length c) b