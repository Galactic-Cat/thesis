{-# LANGUAGE InstanceSigs #-}

module Forward (forward, getName, Forward, Forwarded (FLift, FOp0, FOp1, FOp2, FMap, FMapV, FFold, FFoldV, FJoin), FValue (FArray, FBool, FReal, FFunc)) where
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())

    import Expression (
        EEnvironment,
        EValue (EArray, EBool, EReal),
        Expression (EApply, EIf, ELambda, ELet, ELift, EOp0, EOp1, EOp2, EOp3, ERef),
        Op0 (Iota),
        Op1 (Gen, Idx, Neg, Sin, Sum),
        Op2 (Add, Equ, Gt, Gte, Lt, Lte, Map, Mul, Neq, Sub),
        Op3 (Fold))

    -- Type Definitions --

    type FEnvironment = Map String FValue

    type Forward = Map String (Forwarded, Int, FValue)

    data Forwarded
        = FLift  FValue
        | FOp0   Op0
        | FOp1   Op1       String
        | FOp2   Op2       String String
        | FMap   [Forward] String
        | FMapV  Forward   String
        | FFold  Forward   String String
        | FFoldV Forward   String Forward String String
        | FJoin  [String] -- Special statement for FFoldV
        deriving (Show)

    data FValue
        = FArray String [Float]
        | FBool  String Bool
        | FFunc  Bool   (FValue -> Int -> Forward -> (FValue, Forward, Int))
        | FReal  String Float
        | FWrap  String String

    instance Show FValue where
        show :: FValue -> String
        show (FArray s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (FBool  s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (FFunc  _ _) = "FFunc"
        show (FReal  s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (FWrap  s r) = "(" ++ s ++ " = " ++ r      ++ ")"

    -- Function Definitions --
    -- Counts a reference in the forward pass
    addReference :: String -> Forward -> Forward
    addReference s f = case Map.lookup s f of
        Just (d, c, v) -> Map.insert s (d, c + 1, v) f
        Nothing        ->
            let arrayName = takeWhile (/= '!') s -- Attempt to extract an array name from s
            in  case Map.lookup arrayName f of
                Just (d, c, v) -> Map.insert arrayName (d, c + 1, v) f -- Reference the array instead of one item
                Nothing        -> error $ "Reference '" ++ s ++ "' not in forward map: " ++ show f

    -- Checks if a lambda expression has branches or not
    branchCheck :: FEnvironment -> Expression -> Bool
    branchCheck n (EApply e1 e2)    = branchCheck n e1 || branchCheck n e2
    branchCheck _ (EIf {})          = True
    branchCheck n (ELambda s e1)    = branchCheck (Map.insert s (FReal s 0) n) e1
    branchCheck n (ELet s e1 e2)    =
        let b1 = branchCheck n e1
            b2 = branchCheck (Map.insert s (FReal s 0) n) e2
        in  b1 || b2
    branchCheck _ (ELift _)         = False
    branchCheck _ (EOp0 _)          = False
    branchCheck n (EOp1 _ e1)       = branchCheck n e1
    branchCheck n (EOp2 _ e1 e2)    = branchCheck n e1 || branchCheck n e2
    branchCheck n (EOp3 _ e1 e2 e3) = branchCheck n e1 || branchCheck n e2 || branchCheck n e3
    branchCheck n (ERef s1)         =
        case n Map.! s1 of
            (FFunc b _) -> b
            (FWrap _ r) -> branchCheck n (ERef r)
            _           -> False

    -- Wrapper function for the forward pass
    -- Takes in: input variables as an environment, a boolean to keep array in trace, the expression to pass over
    -- Returns: a tuple containing the output value, and the completed forward pass
    forward :: EEnvironment -> Bool -> Expression -> (FValue, Forward)
    forward n k e = let (v, f, _) = forward' n' k 0 e f' in (v, f)
        where
            n' = switchEnv n
            f' = Map.map lift n'
            lift v = (FLift v, 0, v)

    -- Workhorse function for the forward pass
    -- Takes in: the current environment, a boolean to keep arrays while tracing, a counter for variable names, the current expression, the current forward environment
    forward' :: FEnvironment -> Bool -> Int -> Expression -> Forward -> (FValue, Forward, Int)
    forward' n k c (EApply e1 e2) f =
        let (v1, f1, c1) = forward' n k c  e1 f
            (v2, f2, c2) = forward' n k c1 e2 f1
        in  case v1 of
            FFunc _ a -> a v2 c2 f2
            _         -> error "Type mismatch in forward'/EApply"

    forward' n k c (EIf e1 e2 e3) f =
        let (v1, _, _) = forward' n k c e1 f
        in  case v1 of
            FBool _ True  -> forward' n k c e2 f
            FBool _ False -> forward' n k c e3 f
            _             -> error "Type mismatch in forward'/EIf"

    forward' n k c (ELambda s1 e1) f =
        let b = branchCheck (Map.insert s1 (FReal s1 0) n) e1
            u = FFunc b (\x c' f' -> forward' (Map.insert s1 (FWrap s1 (getName x)) $ Map.insert (getName x) x n) k c' e1 f')
        in  (u, f, c)

    forward' n k c (ELet s1 e1 e2) f =
        let (v1, f1, c1) = forward' n k c e1 f
            n'           = Map.insert s1 (renameValue s1 v1) n -- Make sure the variable has the right name in the environment
        in  case v1 of
            FFunc {} -> forward' n' k c1 e2 f1
            _        -> forward' n' k c1 e2 (rename (getName v1) s1 f1) -- Make sure the variable has the right name in the forward map

    forward' _ k c (ELift v1)     f =
        let s1 = 'r' : show c
        in  case v1 of
            (EArray v) ->
                if   k
                then (FArray s1 v, Map.insert s1 (FLift (FArray s1 v), 0, FArray s1 v) f, c + 1)
                else (FArray s1 v, forwardArrayLift s1 0 v f, c + 1)
            (EBool  v) -> (FBool s1 v, Map.insert s1 (FLift (FBool s1 v), 0, FBool s1 v) f, c + 1)
            (EReal  v) -> (FReal s1 v, Map.insert s1 (FLift (FReal s1 v), 0, FReal s1 v) f, c + 1)
            _          -> error "Type mismatch in forward'/ELift"

    forward' _ k c (EOp0 (Iota i)) f =
        let s1 = 'r' : show c
            v  = FArray s1 [0 .. fromIntegral (i - 1)]
        in  if   k
            then (v, Map.insert s1 (FOp0 (Iota i), 0, v) f, c + 1)
            else (v, forwardArrayLift s1 0 [0 .. fromIntegral (i - 1)] f, c + 1)

    forward' n k c (EOp1 op e1)    f =
        let (v1, f1, c1) = forward' n k c e1 f
            s = 'r' : show c1
        in  case (op, v1) of
            (Gen i, FFunc  b  g) ->
                let v' = [0 .. fromIntegral (i - 1)]
                    v  = FArray s v'
                    s' = 'r' : show (c1 + 1)
                in  if   k
                    then let f2 = Map.insert s (FOp0 (Iota i), 1, v) f1 -- Start with reference counter at 1, because it is referenced by the map operation
                         in  if   b
                             then let (vs, fs, cs) = forwardMapFull g (c1 + 2) s s' v' 0 f2
                                  in  (vs, Map.insert s' (FMap fs s, 0, vs) f2, cs)
                             else let (vs, fs, cs) = forwardMapVect g (c1 + 2) s s' v' 0 f2
                                  in  (vs, Map.insert s' (FMapV fs s, 0, vs) f2, cs)
                    else let f2 = forwardArrayLift s 0 v' f1
                         in  forwardMap g (c1 + 2) s s' v' 0 f2
            (Idx i, FArray s1 a) ->
                if   k
                then (FReal s $ a !! i, addReference s1 $ Map.insert s (FOp1 op s1, 0, FReal s $ a !! i) f1, c1 + 1)
                else let s' = s1 ++ '!' : show i
                     in  (FReal s' $ a !! i, f1, c1)
            (Neg,   FBool  s1 a) -> (FBool s $ not a, addReference s1 $ Map.insert s (FOp1 Neg s1, 0, FBool s $ not a) f1, c1 + 1)
            (Sin,   FReal  s1 a) -> (FReal s $ sin a, addReference s1 $ Map.insert s (FOp1 Sin s1, 0, FReal s $ sin a) f1, c1 + 1)
            (Sum,   FArray s1 a) ->
                if   k
                then (FReal s $ sum a, addReference s1 $ Map.insert s (FOp1 op s1, 0, FReal s $ sum a) f1, c1 + 1)
                else forwardArraySum s1 c1 a f1
            _                    -> error "Type mismatch in forward'/EOp1"

    forward' n k c (EOp2 op e1 e2) f =
        let (v1, f1, c1) = forward' n k c  e1 f
            (v2, f2, c2) = forward' n k c1 e2 f1
            s = 'r' : show c2
        in  case (op, v1, v2) of
            (Add, FReal s1 a, FReal  s2 b) ->
                let v = FReal s $ a + b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Equ, FBool s1 a, FBool  s2 b) ->
                let v = FBool s $ a == b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Equ, FReal s1 a, FReal  s2 b) ->
                let v = FBool s $ a == b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Gt,  FReal s1 a, FReal  s2 b) ->
                let v = FBool s $ a > b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Gte, FReal s1 a, FReal  s2 b) ->
                let v = FBool s $ a >= b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Lt,  FReal s1 a, FReal  s2 b) ->
                let v = FBool s $ a < b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Lte, FReal s1 a, FReal  s2 b) ->
                let v = FBool s $ a <= b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Map, FFunc b g, FArray s2 v') ->
                if   k
                then if   b
                     then let (vs, fs, cs) = forwardMapFull g (c1 + 2) s2 s v' 0 f2
                              uf = foldl (\q1 q2 -> updateSubtraceRefs q1 q2 s2 "") f2 fs
                          in  (vs, Map.insert s (FMap fs s2, 0, vs) (addReference s2 uf), cs)
                     else let (vs, fs, cs) = forwardMapVect g (c1 + 2) s2 s v' 0 f2
                              uf = updateSubtraceRefs f2 fs s2 ""
                          in  (vs, Map.insert s (FMapV fs s2, 0, vs) (addReference s2 uf), cs)
                else forwardMap g (c1 + 2) s2 s v' 0 f2
            (Mul, FReal s1 a, FReal  s2 b) ->
                let v = FReal s $ a * b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Neq, FBool s1 a, FBool  s2 b) ->
                let v = FBool s $ a == b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Neq, FReal s1 a, FReal  s2 b) ->
                let v = FBool s $ a == b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            (Sub, FReal s1 a, FReal  s2 b) ->
                let v = FReal s $ a - b
                in  (v, addReference s1 (addReference s2 (Map.insert s (FOp2 op s1 s2, 0, v) f2)), c2 + 1)
            _                              -> error "Type mismatch in forward'/EOp2"

    forward' n k c (EOp3 Fold e1 e2 e3) f =
        let (v1, f1, c1) = forward' n k c  e1 f
            (v2, f2, c2) = forward' n k c1 e2 f1
            (v3, f3, c3) = forward' n k c2 e3 f2
        in  case (v1, v2, v3) of
            (FFunc b fn, FArray s2 a, FReal s3 _) ->
                if   k
                then if   b
                     then let (vf, ff, cf) = forwardFoldFull (foldFunction fn) c3 s2 v3 a 0 f3
                              uf = updateSubtraceRefs f3 ff s2 s3 -- Update counters from inside the subtrace
                          in  (vf, Map.insert (getName vf) (FFold ff s2 s3, 0, vf) (addReference s2 (addReference s3 uf)), cf)
                     else let (vf, ff1, q, ff2, cf) = forwardFoldVect (foldFunction fn) c3 s2 v3 a f3
                              uf1 = updateSubtraceRefs f3 ff1 s2 s3 -- Update counters from inside the subtrace
                              uf2 = updateSubtraceRefs uf1 ff2 s2 s3
                          in  (vf, Map.insert (getName vf) (FFoldV ff1 q ff2 s2 s3, 0, vf) (addReference s2 (addReference s3 uf2)), cf)
                else forwardFoldFull (foldFunction fn) c3 s2 v3 a 0 f3
            _                                    -> error "Type mismatch in forward'/EOp3"

    forward' n k c (ERef s) f = case Map.lookup s n of
        Just (FWrap _ r) -> forward' n k c (ERef r) f
        Just v           -> (v, f, c)
        Nothing          -> error $ "Key '" ++ s ++ "' missing from environment:\n" ++ show n ++ "\n"

    -- Creates a sum of all values in an array
    -- This function does empty or singleton arrays, or passes it on to forwardArraySum'
    forwardArraySum :: String -> Int -> [Float] -> Forward -> (FValue, Forward, Int)
    forwardArraySum _ c [] f =
        let s = 'r' : show c
        in  (FReal s 0, f, c + 1)

    forwardArraySum s c [v] f =
        let s' = s ++ "!0"
        in  (FReal s' v, f, c)

    forwardArraySum s c (x:y:vs) f =
        let sx = s ++ "!0"
            sy = s ++ "!1"
            s' = 'r' : show c
            f' = Map.insert s' (FOp2 Add sx sy, 0, FReal s' $ x + y) f
        in  forwardArraySum' s (c + 1) vs 2 (FReal s' $ x + y) (addReference sx (addReference sy f'))

    -- Creates a sum of all values in an array, does most of the work when called by forwardArraySum
    forwardArraySum' :: String -> Int -> [Float] -> Int -> FValue -> Forward -> (FValue, Forward, Int)
    forwardArraySum' _ c [] _ a f = (a, f, c)

    forwardArraySum' s c (v:vs) i (FReal sa va) f =
        let s'  = s ++ '!' : show i
            sa' = 'r' : show c
            va' = va + v
            f'  = Map.insert sa' (FOp2 Add sa s', 0, FReal sa' va') (addReference sa (addReference s' f))
        in  forwardArraySum' s (c + 1) vs (i + 1) (FReal sa' va') f'

    forwardArraySum' _ _ (_:_) _ _ _ = error "Type mismatch in forwardArraySum'"

    -- Lifts all values in an array seperately for the forward-trace
    forwardArrayLift :: String -> Int -> [Float] -> Forward -> Forward
    forwardArrayLift _ _ []     f = f
    forwardArrayLift s c (v:vs) f =
        let s1 = s ++ '!' : show c
            v' = FReal s1 v
            f' = Map.insert s1 (FLift v', 0, v') f
        in  forwardArrayLift s (c + 1) vs f'

    -- Creates a binary function from a nested unary one, for easier use in fold
    foldFunction :: (FValue -> Int -> Forward -> (FValue, Forward, Int)) -> (FValue -> FValue -> Int -> Forward -> (FValue, Forward, Int))
    foldFunction fn x y c f = case fn x c f of
        (FFunc _ gn, gf, gc) -> gn y gc gf
        _                    -> error "Type mismatch in foldFunction"

    -- Performs a simple foldr sequentially and traces the result.
    -- Takes in: the folding function, the current name counter, the name of the original array, the identity value,
    --   the float contents of the array, the current index, and the current forward map
    -- Returns: a tuple containing the resulting value, the forward map for the fold, and name counter
    forwardFoldFull :: (FValue -> FValue -> Int -> Forward -> (FValue, Forward, Int)) -> Int -> String -> FValue -> [Float] -> Int -> Forward -> (FValue, Forward, Int)
    forwardFoldFull _  c _  z []     _ f = (z, f, c)
    forwardFoldFull fn c sa z (x:xs) i f =
        let sx           = sa ++ '!' : show i
            x'           = FReal sx x
            (vx, fx, cx) = fn z x' c f
        in  forwardFoldFull fn cx sa vx xs (i + 1) fx

    -- The number of hypothetical threads to fold over with forwardFoldVect
    vectorizedFoldThreads :: Int
    vectorizedFoldThreads = 2

    -- Divides the array over a number of threads, to fold1 them 
    -- Takes in: the folding function, the current name counter, the name of the original array, the identity value,
    --   the float contents of the array, and the current forward map
    -- Returns: a tuple containing the resulting value, the forward maps for the data parallel parts, the forward map for the combination, and name counter
    forwardFoldVect :: (FValue -> FValue -> Int -> Forward -> (FValue, Forward, Int)) -> Int -> String -> FValue -> [Float] -> Forward -> (FValue, Forward, String, Forward, Int)
    forwardFoldVect fn c sa z xs f
        | len < vectorizedFoldThreads =
            let (vxs, fxs, cxs) = forwardFoldFull fn c sa z xs 0 f
                fxs' = Map.singleton (getName vxs) (FFold fxs sa (getName z), 0, vxs)
            in  (vxs, fxs', "", Map.empty, cxs)
        | len < (vectorizedFoldThreads * 2) = controller (len `div` 2) 0 c f [] xs
        | otherwise = controller vectorizedFoldThreads 0 c f [] xs
        where
            len = length xs + 1
            controller :: Int -> Int -> Int -> Forward -> [String] -> [Float] -> (FValue, Forward, String, Forward, Int)
            controller 0 _ c' fs ss vs =
                let s = 'r' : show c'
                    (vc, fc, cc) = forwardFoldFull fn (c' + 1) s (FReal (s ++ "!0") (head vs)) (tail vs) 1 $ Map.insert s (FJoin ss, 0, FArray s vs) f
                in  (vc, fs, s, fc, cc)
            controller t s c' fs ss vs
                | mod (len - s) t == 0 =
                    if   s == 0
                    then let size = len `div` vectorizedFoldThreads
                             sarr = take (size - 1) xs
                             (vf, ff, cf) = forwardFoldFull fn c' sa z sarr 0 f
                             ff' = Map.insert (getName vf) (FFold ff sa (getName z), 1, vf) fs
                         in  controller (t - 1) size cf ff' [getName vf] [unliftFloat vf]
                    else let size = len `div` vectorizedFoldThreads
                             sarr = take size (drop (s - 1) xs)
                             (vf, ff, cf) = forwardFoldFull fn c' sa (FReal (sa ++ '!' : show (s - 1)) (head sarr)) (tail sarr) s f
                             ff' = Map.insert (getName vf) (FFold ff sa (getName z), 1, vf) fs
                         in  controller (t - 1) (s + size) cf ff' (getName vf : ss) (unliftFloat vf : vs)
                | otherwise            =
                    if   s == 0
                    then let size = (len `div` vectorizedFoldThreads) + 1
                             sarr = take (size - 1) xs
                             (vf, ff, cf) = forwardFoldFull fn c' sa z sarr 0 f
                             ff' = Map.insert (getName vf) (FFold ff sa (getName z), 1, vf) fs
                         in  controller (t - 1) size cf ff' [getName vf] [unliftFloat vf]
                    else let size = (len `div` vectorizedFoldThreads) + 1
                             sarr = take size (drop (s - 1) xs)
                             (vf, ff, cf) = forwardFoldFull fn c' sa (FReal (sa ++ '!' : show (s - 1)) (head sarr)) (tail sarr) s f
                             ff' = Map.insert (getName vf) (FFold ff sa (getName z), 1, vf) fs
                         in  controller (t - 1) (s + size) cf ff' (getName vf : ss) (unliftFloat vf : vs)


    -- Performs tracing and caculations of a mapping operations, but whilst tracing away array structure
    -- Takes in: the function to map, the current name counter, the name of the old array, the name of the new array,
    --   the float contents of the array, the current index, and the current forward map
    -- Returns: a tuple containing the resulting array, forward map, and name counter
    forwardMap :: (FValue -> Int -> Forward -> (FValue, Forward, Int)) -> Int -> String -> String -> [Float] -> Int -> Forward -> (FValue, Forward, Int)
    forwardMap _  c _  sn []     _ f = (FArray sn [], f, c)
    forwardMap fn c so sn (x:xs) i f =
        let sox = so ++ '!' : show i
            snx = sn ++ '!' : show i
            v   = FReal sox x
            (xv,  xf,  xc)  = fn v c f
            (xsv, xsf, xsc) = forwardMap fn xc so sn xs (i + 1) xf
        in  case (xv, xsv) of
            (FReal s' v', FArray _ vs) -> (FArray sn (v' : vs), rename s' snx xsf, xsc)
            _                          -> error "Type mismatch in forwardMap"

    -- Calculates values for all items in a mapping operation, but traces only the first one
    -- Takes in: the function to map, the current name counter, the name of the old array, the name of the new array,
    --   the float contents of the array, the current index, and the current forward map
    -- Returns: a tuple containting the value, the resulting forward map, and the new name counter
    forwardMapVect :: (FValue -> Int -> Forward -> (FValue, Forward, Int)) -> Int -> String -> String -> [Float] -> Int -> Forward -> (FValue, Forward, Int)
    forwardMapVect _  c _  sn []     _ _ = (FArray sn [], Map.empty, c)
    forwardMapVect fn c so sn (x:xs) i f =
        if   i == 0
        then let sox = so ++ "!i"
                 snx = sn ++ "!i"
                 v   = FReal sox x
                 (xv,  xf, xc)  = fn v c f
                 (xsv, _,  xsc) = forwardMapVect fn xc so sn xs (i + 1) f
             in  case (xv, xsv) of
                 (FReal s' v', FArray _ vs) -> (FArray sn (v' : vs), rename s' snx xf, xsc)
                 _                          -> error "Type mismatch in forwardMapVect (1)"
        else let sox = so ++ "!i"
                 v   = FReal sox x
                 (xv,  _,   xc)  = fn v c f
                 (xsv, xsf, xsc) = forwardMapVect fn xc so sn xs (i + 1) f
             in  case (xv, xsv) of
                 (FReal _ v', FArray _ vs) -> (FArray sn (v' : vs), xsf, xsc)
                 _                         -> error "Type mismatch in forwardMapVect (2)"

    -- Traces and calculates (intermediate) values for all steps in a mapping operation
    -- Takes in: the function to map, the current name counter, the name of the old array, the name of the new array,
    --   the float contents of the array, the current index, and the current forward map
    -- Returns: a tuple containing the value, the resulting forward map, and the new name counter
    forwardMapFull :: (FValue -> Int -> Forward -> (FValue, Forward, Int)) -> Int -> String -> String -> [Float] -> Int -> Forward -> (FValue, [Forward], Int)
    forwardMapFull _  c _  sn []     _ _ = (FArray sn [], [], c)
    forwardMapFull fn c so sn (x:xs) i f =
        let sox             = so ++ '!' : show i
            snx             = sn ++ '!' : show i
            v               = FReal sox x
            (xv,  xf,  xc)  = fn v c f
            (xsv, xsf, xsc) = forwardMapFull fn xc so sn xs (i + 1) f
        in  case (xv, xsv) of
            (FReal s' v', FArray _ vs) -> (FArray sn (v' : vs), rename s' snx xf : xsf, xsc)
            _                          -> error "Type mismatch in forwardMapFull"

    -- Gets the name from a FValue if it has one
    getName :: FValue -> String
    getName (FArray s _) = s
    getName (FBool  s _) = s
    getName (FReal  s _) = s
    getName (FWrap  _ s) = s
    getName _            = error "Tried to get name of function in getName"

    -- Renames an item in the forward-trace
    rename :: String -> String -> Forward -> Forward
    rename so sn f = case Map.lookup so f of
        Just (e, r, v) -> Map.insert sn (renameForwarded so sn e, r, renameValue sn v) (Map.delete so f)
        Nothing        -> case Map.lookup (so ++ "!0") f of
            Just _  -> renameArray so sn 0 f -- Special case for if arrays are traced away
            Nothing -> f

    -- Renames indexed array items
    renameArray :: String -> String -> Int -> Forward -> Forward
    renameArray so sn i f = case Map.lookup so' f of
        Just (e, r, v) -> renameArray so sn (i + 1) $ Map.insert sn' (renameForwarded so' sn' e, r, renameValue sn' v) (Map.delete so' f)
        Nothing        -> f
        where
            so' = so ++ '!' : show i
            sn' = sn ++ '!' : show i

    renameForwarded :: String -> String -> Forwarded -> Forwarded
    renameForwarded so sn (FLift v)    = FLift $ renameValueSafe so sn v
    renameForwarded so sn (FMap  fs r) = FMap (map (rename so sn) $ renameMap fs 0) r
        where
            renameMap :: [Forward] -> Int -> [Forward]
            renameMap []      _ = []
            renameMap (f:fss) i = rename (so ++ '!' : show i) (sn ++ '!' : show i) f : renameMap fss (i + 1)
    renameForwarded so sn (FMapV f  r) = FMapV (rename so sn $ rename (so ++ "!i") (sn ++ "!i") f) r

    renameForwarded _  _  v            = v -- Only lift operations might need deep renaming

    -- Renames a FValue with a new name, mainly a helper funtion for let bindings
    renameValue :: String -> FValue -> FValue
    renameValue s (FArray _ v) = FArray s v
    renameValue s (FBool  _ v) = FBool s v
    renameValue s (FReal  _ v) = FReal s v
    renameValue s (FWrap  _ r) = FWrap s r
    renameValue _ v@(FFunc {}) = v -- Functions don't carry a name because they're always traced away

    -- Renames a FValue only if it's old name matches the given old name, mainly a helper function forl let bindings
    renameValueSafe :: String -> String -> FValue -> FValue
    renameValueSafe so sn v@(FArray sc _) = if sc == so then renameValue sn v else v
    renameValueSafe so sn v@(FBool  sc _) = if sc == so then renameValue sn v else v
    renameValueSafe so sn v@(FReal  sc _) = if sc == so then renameValue sn v else v
    renameValueSafe so sn v@(FWrap  sc _) = if sc == so then renameValue sn v else v
    renameValueSafe _  _  v               = v -- Functions don't carry a name because they're always traced away

    -- Takes in the input variables as an EEnvironment, and creates an FEnvironment for the forward pass
    switchEnv :: EEnvironment -> FEnvironment
    switchEnv = Map.mapWithKey switch
        where
            switch k (EArray e) = FArray k e
            switch k (EBool  e) = FBool  k e
            switch k (EReal  e) = FReal  k e
            switch _ _          = error "Function in starting environment"

    -- If a FValue is a float, return the float, if not error
    unliftFloat :: FValue -> Float
    unliftFloat (FReal _ v) = v
    unliftFloat _           = error "Type mismatch in unliftFloat"

    updateSubtraceRefs :: Forward -> Forward -> String -> String -> Forward
    updateSubtraceRefs fm fs s1 s2 = fst $ Map.mapAccumRWithKey mapper fm fs
        where
            mapper :: Forward -> String -> (Forwarded, Int, FValue) -> (Forward, (Forwarded, Int, FValue))
            mapper f k v@(_, cs, _)
                | k == s1 || k == s2 = (f, v) -- Don't count marked items
                | otherwise          = case Map.lookup k f of
                    Just (f', cm, v') -> (Map.insert k (f', cm + cs, v') f, v)
                    Nothing           -> (f, v) -- Only update items if they are already in the main trace