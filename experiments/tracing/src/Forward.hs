-- {-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns #-}

module Forward (
    addSuccessor,
    forward,
    Forward,
    Forwarded (FLift, FOp0, FOp1, FOp2, FMap, FMapV)
) where
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())

    import Expression (Op0 (Iota), Op1, Op2)

    import Trace (
        reorderTrace,
        resolveOp1,
        resolveOp2,
        TEnvironment,
        Trace,
        Traced (TLift, TOp0, TOp1, TOp2, TMap, TMapV),
        TValue (TArray, TReal),
        unliftFloat)

    data Forwarded
        = FLift TValue
        | FOp0  Op0
        | FOp1  Op1       String
        | FOp2  Op2       String String
        | FMap  [Forward] String
        | FMapV Forward   String
        deriving (Show)

    -- Stores an trace augmented with succesors and intermediate values
    type Forward = Map String (Forwarded, [String], TValue)

    addSuccessor :: Forward -> String -> String -> Forward
    addSuccessor f r s = Map.insert r (t, s : ss, v) f
        where
            (t, ss, v) = f Map.! r

    -- Performs the forward pass on a trace
    forward :: Trace -> Forward
    forward = fst . forward' Map.empty

    forward' :: TEnvironment -> Trace -> (Forward, TValue)
    forward' _ []              = (Map.empty, TReal "0" 0)
    forward' n tss@((s, t):ts) = case t of
        TLift v        -> (Map.insert s (FLift v, [], v) $ fst (forward' (Map.insert s v n) ts), v)
        TOp0  (Iota i) ->
            let v = TArray s [0 .. fromIntegral i - 1]
            in  (Map.insert s (FOp0 (Iota i), [], v) $ fst (forward' (Map.insert s v n) ts), v)
        TOp1  op s1    ->
            if   Map.member s1 n
            then let v1 = n Map.! s1
                     v  = resolveOp1 op v1 s
                     f  = Map.insert s (FOp1 op s1, [], v) $ fst (forward' (Map.insert s v n) ts)
                 in  (addSuccessor f s1 s, v)
            else forward' n $ reorderTrace tss s1
        TOp2  op s1 s2 ->
            if   Map.member s1 n
            then if   Map.member s2 n
                 then let v1 = n Map.! s1
                          v2 = n Map.! s2
                          v  = resolveOp2 op v1 v2 s
                          f  = Map.insert s (FOp2 op s1 s2, [], v) $ fst (forward' (Map.insert s v n) ts)
                      in  (addSuccessor (addSuccessor f s1 s) s2 s, v)
                 else forward' n $ reorderTrace tss s2
            else forward' n $ reorderTrace tss s1
        TMap  rss s1   ->
            if   Map.member s1 n
            then let v1       = n Map.! s1
                     (mv, mt) = forwardMap n rss s v1 0
                     f        = Map.insert s (FMap mt s1, [], mv) $ fst (forward' (Map.insert s mv n) ts)
                 in  (addSuccessor f s1 s, mv)
            else forward' n $ reorderTrace tss s1
        TMapV rs  s1   ->
            if   Map.member s1 n
            then let v1       = n Map.! s1
                     (mv, mt) = forwardMapV n rs s v1 0
                     f        = Map.insert s (FMapV mt s1, [], mv) $ fst (forward' (Map.insert s mv n) ts)
                 in  (addSuccessor f s1 s, mv)
            else forward' n $ reorderTrace tss s1

    forwardMap :: TEnvironment -> [Trace] -> String -> TValue -> Int -> (TValue, [Forward])
    forwardMap _ []     s _                  _ = (TArray s [], [])
    forwardMap _ _      s (TArray _  [])     _ = (TArray s [], [])
    forwardMap n (t:ts) s (TArray s' (v:vs)) i =
        let si       = s' ++ '!' : show i
            so       = s  ++ '!' : show i
            (ff, fv) = forward' (Map.insert si (TReal si v) n) t
            fvs      = getName fv
            ff'      = renameForward ff fvs so
            (rv, rf) = forwardMap n ts s (TArray s' vs) (i + 1)
        in  case rv of
            TArray _ ns -> (TArray s (unliftFloat fv : ns), ff' : rf)
            _           -> error "Type mismatch in forwardMap (1)"
    forwardMap _ _      _ _                  _ = error "Type mismatch in forwardMap (2)"

    forwardMapV :: TEnvironment -> Trace -> String -> TValue -> Int -> (TValue, Forward)
    forwardMapV _ _ s (TArray _ [])      _ = (TArray s [], Map.empty)
    forwardMapV n t s (TArray s' (v:vs)) i =
        let (ff, fv) = forward' (Map.insert s' (TReal s' v) n) t
            fvs      = getName fv
            ff'      = renameForward ff fvs s
            rv       = fst $ forwardMapV n t s (TArray s' vs) (i + 1)
        in  case rv of 
            TArray _ ns -> (TArray s (unliftFloat fv : ns), ff')
            _           -> error "Type mismatch in forwardMapV (1)"
    forwardMapV _ _ _ _                  _ = error "Type mismatch in forwardMapV (2)"

    getName :: TValue -> String
    getName (TArray s _) = s
    getName (TReal  s _) = s
    getName _            = error "Type mismatch in getName"

    rename :: TValue -> String -> TValue
    rename (TArray _ v) s = TArray s v
    rename (TReal  _ v) s = TReal  s v
    rename _            _ = error "Type mismatch in rename"

    renameForward :: Forward -> String -> String -> Forward
    renameForward fs old new =
        if   Map.member old fs
        then let (f, ss, v) = fs Map.! old
                 v'         = rename v new
             in  Map.insert new (f, ss, v') $ Map.delete old fs
        else fs