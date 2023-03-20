-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Trace (trace) where
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
        EValue (EBool, EFloat),
        Op0 (Iota),
        Op1 (Idx, Neg, Sin, Sum),
        Op2 (Add, Equ, Gt, Gte, Lt, Lte, Map, Mul, Neq, Sub),
        EEnvironment)
    
    data Traced
        = TLift TValue
        | TOp0  Op0
        | TOp1  Op1    String
        | TOp2  Op2    String String
        deriving (Show)

    data TValue = TArray String [Float] | TBool String Bool | TFloat String Float | TFunc (TValue -> Int -> (TValue, Trace, Int))

    instance Show TValue where
        show (TArray s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (TBool  s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (TFloat s v) = "(" ++ s ++ " = " ++ show v ++ ")"
        show (TFunc  _)   = "TFunc"

    type Trace = Map String Traced

    type TEnvironment = Map String TValue

    (<:>) :: (TValue, Trace, Int) -> Trace -> (TValue, Trace, Int)
    (<:>) (v1, t1, i1) t2 = (v1, Map.union t1 t2, i1)

    switchEnv :: EEnvironment -> TEnvironment
    switchEnv = Map.mapWithKey switch
        where
            switch k (EBool e)  = TBool k e
            switch k (EFloat e) = TFloat k e
            switch _ _          = error "Function in starting environment"
    
    trace :: EEnvironment -> Expression -> Bool -> (TValue, Trace)
    trace n e at = let (v, t, _) = trace' n' 0 e at in (v, Map.union t t')
        where n' = switchEnv n
              t' = Map.map TLift n'

    trace' :: TEnvironment -> Int -> Expression -> Bool -> (TValue, Trace, Int)
    trace' n c _  (EApply e1 e2) =
        let (v1, t1, c1) = trace' n c  e1
            (v2, t2, c2) = trace' n c1 e2 <:> t1
        in  case v1 of
            TFunc f -> f v2 c2 <:> t2
            _       -> error "Type mismatch in trace'/EApply"
    trace' n c _  (EIf e1 e2 e3) =
        let (v1, t1, c1) = trace' n c e1
        in  case v1 of
            (TBool _ True)  -> trace' n c1 e2 <:> t1
            (TBool _ False) -> trace' n c1 e3 <:> t1
            _               -> error "Type mismatch in trace'/EIf"
    trace' n c _  (ELambda s1 e1) = (TFunc $ \x c' -> trace' (Map.insert s1 x n) c' e1, Map.empty, c)
    trace' n c _  (ELet s1 e1 e2) = trace' (Map.insert s1 v1 n) c1 e2 <:> t1
        where (v1, t1, c1) = trace' n c e1
    trace' _ c at (ELift v1)      =
        let s1 = 'r' : show c
        in  case v1 of
            (EBool v)  -> (TBool s1 v, Map.singleton s1 $ TLift (TBool s1 v), c + 1)
            (EFloat v) -> (TFloat s1 v, Map.singleton s1 $ TLift (TFloat s1 v), c + 1)
            _          -> error "Type mismatch in trace'/ELift"
    trace' _ c at (EOp0 (Iota i)) =
        let s = 'r' :  show c
            v = TArray s [0 .. (fromIntegral i - 1)]
        in  if   at 
            then (v, Map.singleton s $ TOp0 (Iota i), c + 1)
            else (v, traceArrayLift s 0 [0 .. (fromIntegral i - 1)], c + 1)
    trace' n c (EOp1 op e1)    =
        let (v1, t1, c1) = trace' n c e1
            s = 'r' : show c1
        in  case (op, v1) of
            (Idx i, TArray s1 a) -> (TFloat s $ a !! i, Map.insert s (TOp1 (Idx i) s1) t1, c1 + 1)
            (Neg,   TBool s1 a)  -> (TBool s $ not a, Map.insert s (TOp1 Neg s1) t1, c1 + 1)
            (Sin,   TFloat s1 a) -> (TFloat s $ sin a, Map.insert s (TOp1 Sin s1) t1, c1 + 1)
            (Sum,   TArray s1 a) -> (TFloat s $ sum a, Map.insert s (TOp1 Sum s1) t1, c1 + 1)
            _                    -> error "Type mismatch in trace'/EOp1"
    trace' n c (EOp2 op e1 e2) =
        let (v1, t1, c1) = trace' n c e1
            (v2, t2, c2) = trace' n c1 e2 <:> t1
            s = 'r' : show c2
        in  case (op, v1, v2) of
            (Add, TFloat s1 a, TFloat s2 b) -> (TFloat s $ a + b, Map.insert s (TOp2 Add s1 s2) t2, c2 + 1)
            (Equ, TBool s1 a,  TBool s2 b)  -> (TBool s $ a == b, Map.insert s (TOp2 Equ s1 s2) t2, c2 + 1)
            (Equ, TFloat s1 a, TFloat s2 b) -> (TBool s $ a == b, Map.insert s (TOp2 Equ s1 s2) t2, c2 + 1)
            (Gt,  TFloat s1 a, TFloat s2 b) -> (TBool s $ a > b, Map.insert s (TOp2 Gt s1 s2) t2, c2 + 1)
            (Gte, TFloat s1 a, TFloat s2 b) -> (TBool s $ a >= b, Map.insert s (TOp2 Gte s1 s2) t2, c2 + 1)
            (Lt,  TFloat s1 a, TFloat s2 b) -> (TBool s $ a < b, Map.insert s (TOp2 Lt s1 s2) t2, c2 + 1)
            (Lte, TFloat s1 a, TFloat s2 b) -> (TBool s $ a <= b, Map.insert s (TOp2 Lte s1 s2) t2, c2 + 1)
            (Map, TFunc f,     TArray s2 b) ->
                let (vm, tm, cm) = traceMap n (c + 1) 0 f (TArray s2 b) 
                in  case vm of
                    (TArray _ vmv) -> (TArray s vmv, Map.insert s (TOp2 Map "lambda" s2) (Map.union tm t2), cm)
                    _              -> error "Type mismatch in trace'/EOp2/Map"
            (Mul, TFloat s1 a, TFloat s2 b) -> (TFloat s $ a * b, Map.insert s (TOp2 Mul s1 s2) t2, c2 + 1)
            (Neq, TBool s1 a,  TBool s2 b)  -> (TBool s $ a /= b, Map.insert s (TOp2 Neq s1 s2) t2, c2 + 1)
            (Neq, TFloat s1 a, TFloat s2 b) -> (TBool s $ a /= b, Map.insert s (TOp2 Neq s1 s2) t2, c2 + 1)
            (Sub, TFloat s1 a, TFloat s2 b) -> (TFloat s $ a - b, Map.insert s (TOp2 Sub s1 s2) t2, c2 + 1)
            (_,   _,          _)          -> error "Type mismatch in trace'/EOp2"
    trace' n c (ERef s1) = (n Map.! s1, Map.empty, c)

    traceArrayLift :: String -> Int -> [Float] -> Trace
    traceArrayLift _ _ []     = Map.empty
    traceArrayLift s i (x:xs) =
        let txs = traceArrayLift s (i + 1) xs
            s'  = s ++ '!' : show i
        in  Map.insert s' (TLift (TFloat s' x)) txs

    traceArraySum :: String -> Int -> [Float] -> Trace
    traceArraySum _ _ [_]      = Map.empty
    traceArraySum s i (x:y:xs) =
        where (TFloat)

    traceMap :: TEnvironment -> Int -> Int -> (TValue -> Int -> (TValue, Trace, Int)) -> TValue -> (TValue, Trace, Int)
    traceMap _ c _ _ (TArray s [])     = (TArray s [], Map.empty, c)
    traceMap n c i f (TArray s (x:xs)) =
        let (vx, tx, cx) = f (TFloat (s ++ '!' : show i) x) c
            (vs, ts, cs) = traceMap n cx (i + 1) f (TArray s xs)
        in  case (vx, vs) of
            (TFloat _ v, TArray s' xsv) -> (TArray s' (v : xsv), Map.union tx ts, cs)
            _                           -> error "Type mismatch in traceMap (1)"
    traceMap _ _ _ _ _                 = error "Type mismatch in traceMap (2)"
