-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Reverse (augment, reverse) where
    import Prelude hiding (reverse)
    import qualified Data.Map.Strict as Map
    import Data.Map (Map)

    -- import qualified Debug.Trace as Debug (trace)

    import Expression (Op0 (Iota), Op1, Op2)

    import Trace (
        evalTrace',
        reorderTrace,
        resolveOp1,
        resolveOp2,
        TEnvironment,
        Trace,
        Traced (TLift, TOp0, TOp1, TOp2, TMap, TMapV),
        TValue (TArray, TReal),
        unliftFloat)

    type Augment = [(String, Augmented, TValue)]

    type Dependents = Map String [String]

    type Reverse = Map String [Float]

    data Augmented
        = ALift TValue
        | AOp0  Op0
        | AOp1  Op1       String
        | AOp2  Op2       String String
        | AMap  [Augment] String
        | AMapV Augment   String
        deriving (Show)

    -- Augment takes in an empty TEnvironment and a trace, and augments the trace with intermediate values
    augment :: TEnvironment -> Trace -> Augment
    augment _   []                      = []
    augment n   ((s, TLift v)      :ts) = (s, ALift v, v) : augment (Map.insert s v n) ts
    augment n   ((s, TOp0 (Iota i)):ts) =
        let v  = TArray s [0.0 .. fromIntegral i - 1]
            t' = AOp0 (Iota i)
        in  (s, t', v) : augment (Map.insert s v n) ts
    augment n t@((s, TOp1 op s1)   :ts) =
        case Map.lookup s1 n of
            Just v1 ->
                let v  = resolveOp1 op v1 s
                    t' = AOp1 op s1
                in  (s, t', v) : augment (Map.insert s v n) ts
            Nothing -> augment n (reorderTrace t s1)
    augment n t@((s, TOp2 op s1 s2):ts) =
        case Map.lookup s1 n of
            Just v1 ->
                case Map.lookup s2 n of
                    Just v2 ->
                        let v  = resolveOp2 op v1 v2 s
                            t' = AOp2 op s1 s2
                        in  (s, t', v) : augment (Map.insert s v n) ts
                    Nothing -> augment n (reorderTrace t s2)
            Nothing -> augment n (reorderTrace t s1)
    augment n t@((s, TMap rss s1)  :ts) =
        case Map.lookup s1 n of
            Just (TArray _ v1) ->
                let  t' = AMap (a rss v1 0) s1
                     v  = TArray s $ m rss v1 0
                in   (s, t', v) : augment (Map.insert s v n) ts
                where
                    a :: [Trace] -> [Float] -> Int -> [Augment]
                    a []     _      _ = []
                    a _      []     _ = []
                    a (r:rs) (v:vs) i = augment (Map.insert si (TReal s1 v) n) r : a rs vs (i + 1)
                        where si = s1 ++ '!' : show i
                    m :: [Trace] -> [Float] -> Int -> [Float]
                    m []     _      _ = []
                    m _      []     _ = []
                    m (r:rs) (v:vs) i = unliftFloat (evalTrace' (Map.insert soi (TReal soi v) n) r sni) : m rs vs (i + 1)
                        where
                            soi = s1 ++ '!' : show i
                            sni = s  ++ '!' : show i
            Just _             -> error "Type mismatch in augment/TMap"
            Nothing            -> augment n (reorderTrace t s1)
    augment n t@((s, TMapV r s1)   :ts) =
        case Map.lookup s1 n of
            Just (TArray _ v1) ->
                let r' = augment (Map.insert s1 (TReal s1 (head v1)) n) r
                    v  = TArray s $ m v1
                    t' = AMapV r' s1
                in  (s, t', v) : augment (Map.insert s v n) ts
                where
                    m :: [Float] -> [Float]
                    m []     = []
                    m (v:vs) = unliftFloat (evalTrace' (Map.insert s1 (TReal s1 v) n) r s) : m vs
            Just _             -> error "Type mismatch in augment/TMapV"
            Nothing            -> augment n (reorderTrace t s1)

    depend :: Trace -> Dependents
    depend [] = Map.empty
    depend (x:xs) = case x of
        (s, TOp1 _  d1)    -> dependOn d1 s (depend xs)
        (s, TOp2 _  d1 d2) -> dependOn d1 s (dependOn d2 s (depend xs))
        (s, TMap rs d1)    -> dependOn d1 s (dependOnAll rs (depend xs))
        (s, TMapV r d1)    -> dependOn d1 s (dependOnAll [r] (depend xs))
        _                  -> depend xs
        where
            dependOn :: String -> String -> Dependents -> Dependents
            dependOn d s m = case Map.lookup d m of
                Just ss -> Map.insert d (s:ss) m
                Nothing -> Map.insert d [s] m
            dependOnAll :: [Trace] -> Dependents -> Dependents
            dependOnAll []     m = m
            dependOnAll (r:rs) m = Map.unionWith (++) (depend r) (dependOnAll rs m)

    reverse :: Trace -> Reverse
    reverse t =
        let d = depend t
            a = augment Map.empty t
        in  reverse' a d Map.empty

    reverse' :: Augment -> Dependents -> Reverse -> Reverse
    reverse' [] _ _ = Map.empty
    reverse' ()

    ready :: Dependents -> String -> Reverse -> Bool
    ready d s r = maybe True ready' (Map.lookup s d)
        where
            ready' []     = True
            ready' (x:xs) = Map.member x r || ready' xs

    addTo :: String -> Float -> Reverse -> Reverse
    addTo s v r = case Map.lookup s r of
        Just vs -> Map.insert s (v:vs) r
        Nothing -> Map.insert s [v] r
