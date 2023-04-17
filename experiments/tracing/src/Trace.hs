{-# LANGUAGE InstanceSigs #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Trace (
    evalTrace,
    evalTrace',
    reorderTrace,
    resolveOp1,
    resolveOp2,
    switchEnv,
    trace,
    TEnvironment,
    Trace,
    Traced (TLift, TOp0, TOp1, TOp2, TMap, TMapV),
    TValue (TArray, TBool, TReal),
    unliftArray,
    unliftFloat
) where
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())

    import Expression (
        Expression (
            EApply,
            EIf,
            ELambda,
            ELet,
            ELift,
            EOp0,
            EOp1,
            EOp2,
            ERef
        ),
        EValue (EArray, EBool, EReal),
        Op0 (Iota),
        Op1 (Idx, Neg, Sin, Sum),
        Op2 (Add, Equ, Gt, Gte, Lt, Lte, Map, Mul, Neq, Sub),
        EEnvironment)

    -- import qualified Debug.Trace as Debug (trace)

    data Traced
        = TLift TValue                                              -- Literals
        | TOp0  Op0                                                 -- Nullary operators
        | TOp1  Op1     String                                      -- Unary operators
        | TOp2  Op2     String String                               -- Binary operators
        | TMap  [Trace] String                                      -- Regular or naïve map tracing
        | TMapV Trace   String                                      -- Vectorized map tracing
        deriving (Show)

    data TValue
        = TArray String [Float]
        | TBool  String Bool                                        -- Since Booleans are traced away, they do not need to be named
        | TReal  String Float
        | TFunc  Bool   (TValue -> Int -> (TValue, Trace, Int))     -- The first Bool is a flag for branching, the Int in the function is the counter

    instance Show TValue where
        show :: TValue -> String
        show (TArray s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (TBool  s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (TReal  s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (TFunc  _ _) = "TFunc"

    type Trace = [(String, Traced)]

    type TEnvironment = Map String TValue

    (<:>) :: (TValue, Trace, Int) -> Trace -> (TValue, Trace, Int)
    (<:>) (v1, t1, i1) t2 = (v1, t1 ++ t2, i1)                      -- Add another trace to a tracing output

    switchEnv :: EEnvironment -> TEnvironment
    switchEnv = Map.mapWithKey switch                               -- Switch the environment from regular values to values for tracing
        where
            switch k (EArray e) = TArray k e
            switch k (EBool  e) = TBool  k e
            switch k (EReal  e) = TReal  k e
            switch _ _          = error "Function in starting environment (1)"

    trace :: EEnvironment -> Bool -> Expression -> (TValue, Trace)
    trace n k e = let (v, t, _) = trace' n' 0 k e in (v, t' ++ t)   -- trace' returns the final value, the trace, and a counter element we don't need
        where n' = switchEnv n                                      -- Switch the input environment to a tracing environment
              t' = traceInputs n k                            -- Create a trace for the input variables

    traceInputs :: Map String EValue -> Bool -> Trace
    traceInputs m k = q l
        where
            l = Map.toList m
            q :: [(String, EValue)] -> Trace
            q []     = []
            q (x:xs) = case x of
                (s, EArray va) ->
                    if   k
                    then (s, TLift (TArray s va)) : q xs
                    else traceArrayLift s 0 va ++ q xs
                (_, EBool  _)  -> q xs
                (s, EReal  v)  -> (s, TLift (TReal s v)) : q xs
                _              -> error "Function in starting environment (2)"

    trace' :: TEnvironment -> Int -> Bool -> Expression -> (TValue, Trace, Int)
    trace' n c k (EApply e1 e2) =
        let (v1, t1, c1) = trace' n c  k e1                         -- Start by tracing e1
            (v2, t2, c2) = trace' n c1 k e2 <:> t1                  -- Trace e2 and append the trace of e1
        in  case v1 of
            TFunc _ f -> f v2 c2 <:> t2                             -- If v1 is a function apply t2
            _         -> error "Type mismatch in trace'/EApply"     -- If not report a type error

    trace' n c k (EIf e1 e2 e3) =
        let (v1, _, _) = trace' n c k e1                            -- Get the value of the e1
        in  case v1 of
            (TBool _ True)  -> trace' n c k e2                      -- If it is a Boolean, and it is true, trace e2
            (TBool _ False) -> trace' n c k e3                      -- If it is a Boolean, but it is false, trace e3 instead
            _               -> error "Type mismatch in trace'/EIf"  -- If it is not a Boolean, report a type error

    trace' n c k (ELambda s1 e1) =
        let b = branchCheck (Map.insert s1 (TReal s1 0) n) k e1     -- Insert a psuedo variable for branch checking
            f = TFunc b (\x c' -> trace' (Map.insert s1 x n) c' k e1)
        in  (f, [], c)

    trace' n c k (ELet s1 e1 e2) = trace' (Map.insert s1 v1 n) c1 k e2 <:> t1 -- Add the trace of e1 to that of e2
        where (v1, t1, c1) = trace' n c k e1

    trace' _ c k (ELift v1)      =
        let s1 = 'r' : show c
        in  case v1 of
            (EArray v) ->
                if   k                                                      -- k for [K]eep arrays
                then (TArray s1 v, [(s1, TLift (TArray s1 v))], c + 1)      -- If array tracing is on, keep the array intact as a single lift
                else (TArray s1 v, traceArrayLift s1 0 v, c + 1)            -- If not, lift every value seperately
            (EBool  v) -> (TBool s1 v, [(s1, TLift (TBool s1 v))], c + 1)
            (EReal  v) -> (TReal s1 v, [(s1, TLift (TReal s1 v))], c + 1)
            _          -> error "Type mismatch in trace'/ELift"

    trace' _ c k (EOp0 (Iota i)) =
        let s = 'r' :  show c
            v = TArray s [0 .. (fromIntegral i - 1)]
        in  if   k
            then (v, [(s, TOp0 (Iota i))], c + 1)                               -- If array tracing is on, keep the iota as a single array-level operation
            else (v, traceArrayLift s 0 [0.0 .. (fromIntegral i - 1)], c + 1)   -- Otherwise, lift every value separtely

    trace' n c k (EOp1 op e1)    =
        let (v1, t1, c1) = trace' n c k e1                          -- Trace e1 first
            s = 'r' : show c1                                       -- Create a name for later use
        in  case (op, v1) of
            (Idx i, TArray s1 a) ->
                let s' = s1 ++ '!' : show i
                in  if   k
                    then (TReal s' $ a !! i, (s, TOp1 op s1) : t1, c1 + 1)  -- Keep indexing in the trace if we're also tracing arrays
                    else (TReal s' $ a !! i, t1, c1)                        -- Otherwise indexing is just variable reference, and does not end in the array
            (Neg,   TBool  s1 a) -> (TBool s $ not a, (s, TOp1 Neg s1) : t1, c1 + 1)
            (Sin,   TReal  s1 a) -> (TReal s $ sin a, (s, TOp1 Sin s1) : t1, c1 + 1)
            (Sum,   TArray s1 a) ->
                if   k
                then (TReal s $ sum a, (s, TOp1 Sum s1) : t1, c1 + 1)   -- Keep sum in the trace if array tracing is enabled
                else traceArraySum c1 v1                                -- Otherwise trace array sums step by step
            _                    -> error "Type mismatch in trace'/EOp1"

    trace' n c k (EOp2 op e1 e2) =
        let (v1, t1, c1) = trace' n c k e1                          -- Trace e1 and e2 first
            (v2, t2, c2) = trace' n c1 k e2 <:> t1                  -- Trace e2 and add e1's trace on to e2's immediately
            s = 'r' : show c2                                       -- Get a name ready for this operation
        in  case (op, v1, v2) of
            (Add, TReal s1 a, TReal  s2 b) -> (TReal s $ a +  b, (s, TOp2 Add s1 s2) : t2, c2 + 1)
            (Equ, TBool s1 a, TBool  s2 b) -> (TBool s $ a == b, (s, TOp2 Equ s1 s2) : t2, c2 + 1)
            (Equ, TReal s1 a, TReal  s2 b) -> (TBool s $ a == b, (s, TOp2 Equ s1 s2) : t2, c2 + 1)
            (Gt,  TReal s1 a, TReal  s2 b) -> (TBool s $ a >  b, (s, TOp2 Gt  s1 s2) : t2, c2 + 1)
            (Gte, TReal s1 a, TReal  s2 b) -> (TBool s $ a >= b, (s, TOp2 Gte s1 s2) : t2, c2 + 1)
            (Lt,  TReal s1 a, TReal  s2 b) -> (TBool s $ a <  b, (s, TOp2 Lt  s1 s2) : t2, c2 + 1)
            (Lte, TReal s1 a, TReal  s2 b) -> (TBool s $ a <= b, (s, TOp2 Lte s1 s2) : t2, c2 + 1)
            (Map, TFunc br f, TArray s2 b) ->
                if   k
                then let (vs, ts, cs) = traceMapNaive f (c2 + 1) s2 s b 0 -- Get the actual value and trace of the map operation
                     in  if   br
                         then (vs, (s, TMap ts s2) : t2, cs)        -- If branches are present in the function, use the naive method
                         else let t = vectorizeTrace s2 s (head ts) -- Otherwise, create a vectorized trace
                              in  (vs, (s, TMapV t s2) : t2, cs)
                else traceArrayMap f c2 s2 s b 0
            (Mul, TReal s1 a, TReal  s2 b) -> (TReal s $ a *  b, (s, TOp2 Mul s1 s2) : t2, c2 + 1)
            (Neq, TBool s1 a, TBool  s2 b) -> (TBool s $ a /= b, (s, TOp2 Neq s1 s2) : t2, c2 + 1)
            (Neq, TReal s1 a, TReal  s2 b) -> (TBool s $ a /= b, (s, TOp2 Neq s1 s2) : t2, c2 + 1)
            (Sub, TReal s1 a, TReal  s2 b) -> (TReal s $ a -  b, (s, TOp2 Sub s1 s2) : t2, c2 + 1)
            (_,   _,          _)          -> error "Type mismatch in trace'/EOp2"
    trace' n c _ (ERef s1) = (n Map.! s1, [], c)

    branchCheck :: TEnvironment -> Bool -> Expression -> Bool
    branchCheck n k (EApply e1 e2) = branchCheck n k e1 || branchCheck n k e2
    branchCheck _ _ (EIf {})       = True
    branchCheck n k (ELambda s e1) = branchCheck (Map.insert s (TReal s 0) n) k e1 -- Insert a psuedo-value so we can safely branch check
    branchCheck n k (ELet s e1 e2) =
        let (v1, _, _) = trace' n 0 k e1                            -- We need the value v1 to branch-check e2
            b1         = branchCheck n k e1
            b2         = branchCheck (Map.insert s v1 n) k e2
        in  b1 || b2
    branchCheck _ _ (ELift _) = False
    branchCheck _ _ (EOp0 _)  = False
    branchCheck n k (EOp1 _ e1) = branchCheck n k e1
    branchCheck n k (EOp2 _ e1 e2) = branchCheck n k e1 || branchCheck n k e2
    branchCheck n _ (ERef s1) =
        case n Map.! s1 of
            (TFunc b _) -> b
            _           -> False

    traceArrayLift :: String -> Int -> [Float] -> Trace
    traceArrayLift _ _ []     = []                                  -- Empty array = empty trace
    traceArrayLift s i (x:xs) =
        let txs = traceArrayLift s (i + 1) xs                       -- Lift the rest of the array
            s'  = s ++ '!' : show i                                 -- Get the name of the current item
        in  (s', TLift (TReal s' x)) : txs                          -- Combine the trace of this item with that of the rest

    traceArraySum :: Int -> TValue -> (TValue, Trace, Int)
    traceArraySum c (TArray _ []) =
        let s = 'r' : show c
        in  (TReal s 0, [(s, TLift (TReal s 0))], c + 1)            -- Empty arrays sum to zero

    traceArraySum c (TArray _ [x]) =
        let s = 'r' : show c
        in  (TReal s 0, [(s, TLift (TReal s x))], c + 1)            -- Singleton arrays sum to that one value

    traceArraySum c (TArray sa (x:y:z)) =
        let sx = sa ++ "!0"                                         -- Get the name for the first item in the array
            sy = sa ++ "!1"                                         -- Get the name for the second item in the array
            s  = 'r' : show c                                       -- Get the name for the addition of the first and second item
            (rv, rt, rc) = traceArraySum' (c + 1) (TArray sa z) 2 (TReal s $ x + y) -- Get the sum on the rest of the array
        in  (rv, (s, TOp2 Add sx sy) : rt, rc)                      -- Return the final result, the trace plus the first addition, and the right count

    traceArraySum _ _ = error "Type mismatch in traceArraySum"      -- Catches all other patterns of TValue

    traceArraySum' :: Int -> TValue -> Int -> TValue -> (TValue, Trace, Int)
    traceArraySum' c (TArray _  [])     _ v            = (v, [], c)  -- No more additions to do, just return the result

    traceArraySum' c (TArray sa (x:xs)) i (TReal sr r) =
        let sx = sa ++ '!' : show i                                  -- Get the name for x
            s  = 'r' : show c                                        -- Get the name for this addition
            (rv, rt, rc) = traceArraySum' (c + 1) (TArray sa xs) (i + 1) (TReal s $ r + x) -- Trace the rest of the addition
        in  (rv, (s, TOp2 Add sr sx) : rt, rc)                       -- Add this addition to the trace on the return pass

    traceArraySum' _ _                  _ _            = error "Type mismatch in traceArraySum'" -- Catch all other patterns of TValue

    -- traceArrayMap takes: the function, the current trace counter, the name of the old array, the name of the new array, the contents of the old array, and the current index
    traceArrayMap :: (TValue -> Int -> (TValue, Trace, Int)) -> Int -> String -> String -> [Float] -> Int -> (TValue, Trace, Int)
    traceArrayMap _ c _  sn []     _ = (TArray sn [], [], c)
    traceArrayMap f c so sn (x:xs) i =
        let r = TReal (so ++ '!' : show i) x                        -- Get the current value
            (fv, ft, fc) = f r c
            (xsv, xst, xsc) = traceArrayMap f fc so sn xs (i + 1)
        in  case (fv, xsv) of
            (TReal s' v, TArray _ xsv') ->
                let vn = TArray sn (v: xsv')
                    ft' = rename s' (sn ++ '!' : show i) ft
                in  (vn, ft' ++ xst, xsc)
            _                           -> error "Type mismatch in traceArrayMap"

    traceMapNaive :: (TValue -> Int -> (TValue, Trace, Int)) -> Int -> String -> String -> [Float] -> Int -> (TValue, [Trace], Int)
    traceMapNaive _ c _  sn []     _ = (TArray sn [], [], c)
    traceMapNaive f c so sn (x:xs) i =
        let old = so ++ '!' : show i
            new = sn ++ '!' : show i
            (xv, xt, xc) = f (TReal old x) c
            (xsv, xst, xsc) = traceMapNaive f xc so sn xs (i + 1)
        in  case (xv, xsv) of
            (TReal s' v, TArray _ vs) ->
                let xt' = rename s' new xt
                in  (TArray sn (v : vs), xt' : xst, xsc)
            _                         -> error "Type mismatch in traceMapNaive"

    vectorizeTrace :: String -> String -> Trace -> Trace
    vectorizeTrace so sn t = deepRename iso so (deepRename isn sn t)
        where iso = so ++ "!0"
              isn = sn ++ "!0"

    rename :: String -> String -> Trace -> Trace
    rename _  _  []          = []
    rename so sn ((s, x):xs) =
        if   s == so
        then (sn, x) : xs
        else (s,  x) : rename so sn xs

    deepRename :: String -> String -> Trace -> Trace
    deepRename _ _ [] = []
    deepRename so sn ((s, x):xs) = (s', x') : deepRename so sn xs
        where
            s' = if s == so then sn else s
            x' = case x of
                TOp1  op sx    -> if sx == so then TOp1 op sn else x
                TOp2  op sx sy -> TOp2 op (if sx == so then sn else sx) (if sy == so then sn else sy)
                TMap  ts sx    -> if sx == so then TMap ts sn else x
                TMapV t  sx    -> if sx == so then TMapV t sn else x
                _              -> x

    evalTrace :: Trace -> String -> TValue
    evalTrace = evalTrace' Map.empty

    evalTrace' :: TEnvironment -> Trace -> String -> TValue
    evalTrace' n []              s = n Map.! s
    evalTrace' n t@((s', t'):ts) s =
        case t' of
            TLift v       -> evalTrace' (Map.insert s' v n) ts s
            TOp0 (Iota i) -> evalTrace' (Map.insert s' (TArray s' [0.0 .. (fromIntegral i - 1)]) n) ts s
            TOp1 op s1    -> 
                case Map.lookup s1 n of
                    Just v  -> evalTrace' (Map.insert s' (resolveOp1 op v s') n) ts s
                    Nothing -> evalTrace' n (reorderTrace t s1) s
            TOp2 op s1 s2 ->
                case Map.lookup s1 n of
                    Just v1 -> case Map.lookup s2 n of
                        Just v2 -> evalTrace' (Map.insert s' (resolveOp2 op v1 v2 s') n) ts s
                        Nothing -> evalTrace' n (reorderTrace t s2) s
                    Nothing -> evalTrace' n (reorderTrace t s1) s
            TMap rss s1 ->
                case Map.lookup s1 n of
                    Just (TArray _ v1) -> evalTrace' (Map.insert s' (TArray s' $ m rss v1 (0 :: Int)) n) ts s
                        where
                            m []     _      _ = []
                            m _      []     _ = []
                            m (r:rs) (v:vs) i = unliftFloat (evalTrace' (Map.insert soi (TReal soi v) n) r sni) : m rs vs (i + 1)
                                where
                                    soi = s1 ++ '!' : show i
                                    sni = s' ++ '!' : show i
                    Just _             -> error "Type mismatch in evalTrace'/TMap"
                    Nothing            -> evalTrace' n (reorderTrace t s1) s
            TMapV r s1 ->
                case Map.lookup s1 n of
                    Just (TArray _ v1) -> evalTrace' (Map.insert s' (TArray s' $ m v1) n) ts s
                        where
                            m [] = []
                            m (v:vs) = unliftFloat (evalTrace' (Map.insert s1 (TReal s1 v) n) r s') : m vs
                    Just _             -> error "Type mismatch in evalTrace'/TMapV"
                    Nothing            -> evalTrace' n (reorderTrace t s1) s

    reorderTrace :: Trace -> String -> Trace
    reorderTrace []            s' = error $ "Cannot reorder empty trace in reorderTrace; tried to get:" ++ s'
    reorderTrace (t@(s, _):ts) s'
        | s == s'   = t:ts
        | otherwise =
            case reorderTrace ts s' of
                []       -> t : ts
                [t']     -> [t', t]
                (t':ts') -> t' : t : ts'

    resolveOp1 :: Op1 -> TValue -> String -> TValue
    resolveOp1 op v1 s = case (op, v1) of
        (Idx i, TArray _ a) -> TReal s (a !! i)
        (Neg,   TBool  _ a) -> TBool s (not a)
        (Sin,   TReal  _ a) -> TReal s (sin a)
        (Sum,   TArray _ a) -> TReal s (sum a)
        _                   -> error "Type mismatch in resolveOp1"

    resolveOp2 :: Op2 -> TValue -> TValue -> String -> TValue
    resolveOp2 op v1 v2 s = case (op, v1, v2) of
        (Add, TReal _ a, TReal  _ b) -> TReal s (a +  b)
        (Equ, TBool _ a, TBool  _ b) -> TBool s (a == b)
        (Equ, TReal _ a, TReal  _ b) -> TBool s (a == b)
        (Gt,  TReal _ a, TReal  _ b) -> TBool s (a >  b)
        (Gte, TReal _ a, TReal  _ b) -> TBool s (a >= b)
        (Lt,  TReal _ a, TReal  _ b) -> TBool s (a <  b)
        (Lte, TReal _ a, TReal  _ b) -> TBool s (a <= b)
        (Map, TFunc _ f, TArray _ b) -> TArray s $ map ((\(v, _, _) -> unliftFloat v) . (`f` 0) . TReal "") b
        (Mul, TReal _ a, TReal  _ b) -> TReal s (a *  b)
        (Neq, TBool _ a, TBool  _ b) -> TBool s (a /= b)
        (Neq, TReal _ a, TReal  _ b) -> TBool s (a /= b)
        (Sub, TReal _ a, TReal  _ b) -> TReal s (a -  b)
        _                            -> error "Type mismatch in resolveOp2"

    unliftArray :: TValue -> [Float]
    unliftArray (TArray _ v) = v
    unliftArray _            = error "Type mismatch in unliftArray"
    
    unliftFloat :: TValue -> Float
    unliftFloat (TReal _ v) = v
    unliftFloat _           = error "Type mismatch in unliftFloat"