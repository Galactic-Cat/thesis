{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AltForward (forward, Forward, Forwarded (FLift, FOp0, FOp1, FOp2, FMap, FMapV), FValue (FArray, FBool, FReal, FFunc)) where
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())

    import Expression (
        EEnvironment,
        EValue (EArray, EBool, EReal),
        Expression (EApply, EIf, ELambda, ELet, ELift, EOp0, EOp1, EOp2, ERef),
        Op0 (Iota),
        Op1 (Gen, Idx, Neg, Sin, Sum),
        Op2 (Add, Equ, Gt, Gte, Lt, Lte, Map, Mul, Neq, Sub))

    -- Type Definitions --

    type FEnvironment = Map String FValue

    type Forward = Map String (Forwarded, Int, FValue)
    
    data Forwarded
        = FLift FValue
        | FOp0  Op0
        | FOp1  Op1     String
        | FOp2  Op2     String String
        | FMap  Forward String
        | FMapV Forward String
        deriving (Show)

    data FValue
        = FArray String [Float]
        | FBool  String Bool
        | FReal  String Float
        | FFunc  Bool   (FValue -> Int -> Forward -> (FValue, Forward, Int))

    instance Show FValue where
        show :: FValue -> String
        show (FArray s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (FBool  s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (FReal  s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (FFunc  _ _) = "FFunc"
    
    -- Function Definitions --
    -- Counts a reference in the forward pass
    addReference :: String -> Forward -> Forward
    addReference s f = case Map.lookup s f of
        Just (d, c, v) -> Map.insert s (d, c + 1, v) f
        Nothing        -> error "Reference not in forward map"

    -- Checks if a lambda expression has branches or not
    branchCheck :: FEnvironment -> Expression -> Bool
    branchCheck n (EApply e1 e2) = branchCheck n e1 || branchCheck n e2
    branchCheck _ (EIf {})       = True
    branchCheck n (ELambda s e1) = branchCheck (Map.insert s (FReal s 0) n) e1
    branchCheck n (ELet s e1 e2) =
        let b1 = branchCheck n e1
            b2 = branchCheck (Map.insert s (FReal s 0) n) e2
        in  b1 || b2
    branchCheck _ (ELift _)      = False
    branchCheck _ (EOp0 _)       = False
    branchCheck n (EOp1 _ e1)    = branchCheck n e1
    branchCheck n (EOp2 _ e1 e2) = branchCheck n e1 || branchCheck n e2
    branchCheck n (ERef s1)      =
        case n Map.! s1 of
            (FFunc b _ ) -> b
            _            -> False

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
            _         -> error "Type error in forward'/EApply"
    
    forward' n k c (EIf e1 e2 e3) f =
        let (v1, _, _) = forward' n k c e1 f
        in  case v1 of
            FBool _ True  -> forward' n k c e2 f
            FBool _ False -> forward' n k c e3 f

    forward' n k c (ELambda s1 e1) f =
        let b = branchCheck (Map.insert s1 (FReal s1 0) n) e1
            u = FFunc b (\x c' f' -> forward' (Map.insert s1 x n) k c' e1 f')
        in  (u, f, c)

    forward' n k c (ELet s1 e1 e2) f =
        let (v1, f1, c1) = forward' n k c e1 f
            n'           = Map.insert s1 v1 n
        in  forward' n' k c1 e2 f1

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
                let v = FArray s [0 .. fromIntegral (i - 1)]
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
            (Map, FFunc br fn, FArray s2 b) ->
                if   k
                then let (vs, fs, cs) = forwardMapFull fn (c2 + 1) s2 s b 0 f2
                     in  if   br
                         then (vs, addReference s2 (Map.insert s (FMap fs s2, 0, vs) f), cs + 1)
                         else let fs' = vectorizeForward
                else _
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
            _                             -> error "Type mismatch in forward'/EOp2"

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
        in  forwardArraySum' s (c + 1) vs 2 (FReal s $ x + y) (addReference sx (addReference sy f'))

    -- Creates a sum of all values in an array, does most of the work when called by forwardArraySum
    forwardArraySum' :: String -> Int -> [Float] -> Int -> FValue -> Forward -> (FValue, Forward, Int)
    forwardArraySum' _ c [] _ a f = (a, f, c)
    
    forwardArraySum' s c (v:vs) i (FReal sa va) f =
        let s'  = s ++ '!' : show i
            sa' = 'r' : show c
            va' = va + v
            f'  = Map.insert sa' (FOp2 Add sa s', 0, FReal sa' va') (addReference sa (addReference s' f))
        in  forwardArraySum' s (c + 1) vs (i + 1) (FReal sa' va') f'

    -- Lifts all values in an array seperately for the forward-trace
    forwardArrayLift :: String -> Int -> [Float] -> Forward -> Forward
    forwardArrayLift _ _ []     f = f
    forwardArrayLift s c (v:vs) f =
        let s1 = s ++ '!' : show c
            v' = FReal s1 v
            f' = Map.insert s1 (FLift v', 0, v') f
        in  forwardArrayLift s (c + 1) vs f'

    forwardMapAlt :: (FValue -> Int -> Forward -> (FValue, Forward, Int)) -> Int -> String -> String -> [Float] -> Int -> Forward -> (FValue, Foward, Int)
    forwardMapAlt _  c _  sn []     _ f = (FArray sn [], f, c)
    forwardMapAlt fn c so sn (x:xs) i f =
        let sox = 'r' : show c
            snx = sn ++ '!' : show i
            v   = FReal sox x
            (xv,  xf,  xc)  = fn v (c + 1) (Map.insert sox (FLift v, 0, ))

    -- Traces and calculates (intermediate) values for all steps in a mapping operation
    -- Takes in: the function to map, the current name counter, the name of the old array, the name of the new array,
    --   the float contents of the array, the current index, and the current forward map
    -- Returns: a tuple containing the value, the resulting forward map, and the new name counter
    forwardMapFull :: (FValue -> Int -> Forward -> (FValue, Forward, Int)) -> Int -> String -> String -> [Float] -> Int -> Forward -> (FValue, Forward, Int)
    forwardMapFull _  c _  sn []     _ f = (FArray sn [], f, c)
    forwardMapFull fn c so sn (x:xs) i f =
        let old             = so ++ '!' : show i
            new             = sn ++ '!' : show i
            (xv,  xf,  xc)  = fn (FReal old x) c f
            (xsv, xsf, xsc) = forwardMapFull fn xc so sn xs (i + 1) xf
        in  case (xv, xsv) of
            (FReal s' v, FArray _ vs) -> (FArray sn (v : vs), rename s' new xsf, xsc)
            _                         -> error "Type mismatch in forwardMapFull"

    vectorizeForward :: String -> String -> Forward -> Forward
    vectorizeForward so sn 0 f =


    -- Renames an item in the forward-trace
    rename :: String -> String -> Forward -> Forward
    rename so sn f = case Map.lookup so f of 
        Just v  -> Map.insert sn v (Map.delete so f)
        Nothing -> f

    -- Takes in the input variables as an EEnvironment, and creates an FEnvironment for the forward pass
    switchEnv :: EEnvironment -> FEnvironment
    switchEnv = Map.mapWithKey switch
        where
            switch k (EArray e) = FArray k e
            switch k (EBool  e) = FBool  k e
            switch k (EReal  e) = FReal  k e
            switch _ _          = error "Function in starting environment"