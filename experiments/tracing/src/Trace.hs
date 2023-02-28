{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
            EOp1,
            EOp2,
            ERef
        ),
        EValue (EBool, EFloat),
        Op1 (Neg, Sin),
        Op2 (Add, Equ, Gt, Gte, Lt, Lte, Mul, Neq, Sub),
        EEnvironment)
    
    data Traced
        = TLift TValue
        | TOp1  Op1    TValue
        | TOp2  Op2    TValue TValue
        | TRef  String
        deriving (Show)

    data TValue = TBool String Bool | TFloat String Float | TFunc (TValue -> Int -> (TValue, Trace, Int))

    instance Show TValue where
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
    
    trace :: EEnvironment -> Expression -> (TValue, Trace)
    trace n e = let (v, t, _) = trace' n' 0 e in (v, Map.union t t')
        where n' = switchEnv n
              t' = Map.map TLift n'

    trace' :: TEnvironment -> Int -> Expression -> (TValue, Trace, Int)
    trace' n c (EApply e1 e2) =
        let (v1, t1, c1) = trace' n c  e1
            (v2, t2, c2) = trace' n c1 e2 <:> t1
        in  case v1 of
            TFunc f -> f v2 c2 <:> t2
    trace' n c (EIf e1 e2 e3) =
        let (v1, t1, c1) = trace' n c e1
        in  case v1 of
            (TBool _ True)  -> trace' n c1 e2 <:> t1
            (TBool _ False) -> trace' n c1 e3 <:> t1
            _               -> error "Type mismatch in trace'/EIf"
    trace' n c (ELambda s1 e1) = (TFunc $ \x c' -> trace' (Map.insert s1 x n) c' e1, Map.empty, c)
    trace' n c (ELet s1 e1 e2) = trace' (Map.insert s1 v1 n) c1 e2 <:> t1
        where (v1, t1, c1) = trace' n c e1
    trace' _ c (ELift v1)      =
        let s1 = 'r' : show c
        in  case v1 of
            (EBool v)  -> (TBool s1 v, Map.singleton s1 $ TLift (TBool s1 v), c + 1)
            (EFloat v) -> (TFloat s1 v, Map.singleton s1 $ TLift (TFloat s1 v), c + 1)
            _          -> error "Type mismatch in trace'/ELift"
    trace' n c (EOp1 op e1)    =
        let (v1, t1, c1) = trace' n c e1
            s = 'r' : show c1
        in  case (op, v1) of
            (Neg, TBool _ a)  -> (TBool s $ not a, Map.insert s (TOp1 Neg v1) t1, c1 + 1)
            (Sin, TFloat _ a) -> (TFloat s $ sin a, Map.insert s (TOp1 Sin v1) t1, c1 + 1)
            (_,   _)          -> error "Type mismatch in trace'/EOp1"
    trace' n c (EOp2 op e1 e2) =
        let (v1, t1, c1) = trace' n c e1
            (v2, t2, c2) = trace' n c1 e2 <:> t1
            s = 'r' : show c2
        in  case (op, v1, v2) of
            (Add, TFloat _ a, TFloat _ b) -> (TFloat s $ a + b, Map.insert s (TOp2 Add v1 v2) t2, c2 + 1)
            (Equ, TBool _ a,  TBool _ b)  -> (TBool s $ a == b, Map.insert s (TOp2 Equ v1 v2) t2, c2 + 1)
            (Equ, TFloat _ a, TFloat _ b) -> (TBool s $ a == b, Map.insert s (TOp2 Equ v1 v2) t2, c2 + 1)
            (Gt,  TFloat _ a, TFloat _ b) -> (TBool s $ a > b, Map.insert s (TOp2 Gt v1 v2) t2, c2 + 1)
            (Gte, TFloat _ a, TFloat _ b) -> (TBool s $ a >= b, Map.insert s (TOp2 Gte v1 v2) t2, c2 + 1)
            (Lt,  TFloat _ a, TFloat _ b) -> (TBool s $ a < b, Map.insert s (TOp2 Lt v1 v2) t2, c2 + 1)
            (Lte, TFloat _ a, TFloat _ b) -> (TBool s $ a <= b, Map.insert s (TOp2 Lte v1 v2) t2, c2 + 1)
            (Mul, TFloat _ a, TFloat _ b) -> (TFloat s $ a * b, Map.insert s (TOp2 Mul v1 v2) t2, c2 + 1)
            (Neq, TBool _ a,  TBool _ b)  -> (TBool s $ a /= b, Map.insert s (TOp2 Neq v1 v2) t2, c2 + 1)
            (Neq, TFloat _ a, TFloat _ b) -> (TBool s $ a /= b, Map.insert s (TOp2 Neq v1 v2) t2, c2 + 1)
            (Sub, TFloat _ a, TFloat _ b) -> (TFloat s $ a - b, Map.insert s (TOp2 Sub v1 v2) t2, c2 + 1)
            (_,   _,          _)          -> error "Type mismatch in trace'/EOp2"
    trace' n c (ERef s1) = (n Map.! s1, Map.empty, c)