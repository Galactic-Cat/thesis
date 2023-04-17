{-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns #-}

module Reverse (reverse) where
    import Prelude hiding (reverse)    
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())

    import Expression (
        Op1 (Idx, Sin, Sum),
        Op2 (Add, Mul, Sub))

    import Trace (
        TValue (TArray, TReal))

    import Forward (
        Forward,
        Forwarded (FLift, FOp0, FOp1, FOp2, FMap, FMapV))

    -- Partial derivatives from arrays, or real numbers, or missing
    data Reversed = RArray [Float] | RReal Float | RNotC
    -- Stores for each step seeding values and its partial derivate (can be null)
    type Reverse = Map String ([Reversed], Reversed)

    -- Outer wrapper for tracing reverse AD on some real output
    reverse :: Forward -> String -> Float -> Reverse
    reverse f s o =
        let r = Map.singleton s ([RReal o], RNotC)
        in  reverse' f [s] r

    reverse' :: Forward -> [String] -> Reverse -> Reverse
    reverse' _ []     r = r
    reverse' f (s:ss) r =
        case getSeed f r s of
            Left (RReal o) ->
                let (fd, _, _) = f Map.! s
                    (r', a)    = seedAncestors f (setSeed r s (RReal o)) (RReal o) fd
                in  reverse' f (ss ++ a) r'
            Left (RArray _) -> error "NYI in reverse'"
            Right md ->
                let ss' = reorder (s:ss) md
                in  reverse' f ss' r

    getSeed :: Forward -> Reverse -> String -> Either Reversed String
    getSeed f r s =
        let (_, dss, _) = f Map.! s
            ss          = q dss
        in  case ss of
            Left  ss' -> Left (RReal $ sum ss')
            Right md  -> Right md
        where
            q :: [String] -> Either [Float] String
            q []     = Left []
            q (d:ds) = case r Map.! d of
                (_, RReal  v) -> case q ds of
                    Left  vs -> Left $ v : vs
                    Right md -> Right md
                (_, RArray _) -> error "NYI in getSeed"
                (_, RNotC)    -> Right d

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
                    let r1 = addSeed r s1 (RReal rv)
                    in  (addSeed r1 s2 (RReal rv), [s1, s2])
                (Mul, TReal _ va, TReal _ vb, RReal rv) ->
                    let r1 = addSeed r s1 (RReal $ rv * vb)
                    in  (addSeed r1 s2 (RReal $ rv * va), [s1, s2])
                (Sub, TReal _ _,  TReal _ _,  RReal rv) ->
                    let r1 = addSeed r s1 (RReal rv)
                    in  (addSeed r1 s2 (RReal rv), [s1, s2])
                _                                       -> error "Type mismatch in seedAncestors/FOp2"

    addSeed :: Reverse -> String -> Reversed -> Reverse
    addSeed r s1 v = case Map.lookup s1 r of
        Just (vs, o) -> Map.insert s1 (v : vs, o) r
        Nothing      -> Map.insert s1 ([v], RNotC) r

    ohe :: Int -> [Float] -> [Float]
    ohe _ 0      = []
    ohe 0 (x:xs) = x : ohe (-1)    xs
    ohe t (_:xs) = 0 : ohe (t - 1) xs

    setSeed :: Reverse -> String -> Reversed -> Reverse
    setSeed r s o = case Map.lookup s r of
        Just (rs, _) -> Map.insert s (rs, o) r
        Nothing      -> Map.insert s ([], o) r

    reorder :: [String] -> String -> [String]
    reorder []       _ = []
    reorder [x]      _ = [x]
    reorder l@(x:xs) s
        | x == s    = l
        | otherwise = case reorder xs s of
            []     -> l
            [y]    -> [y, x]
            (y:ys) -> y : x : ys