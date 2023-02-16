{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Unt (
    Environment,
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
    Op1 (Neg, Sin),
    Op2 (Add, Equ, Gt, Gte, Lt, Lte, Mul, Neq, Sub),
    Trace (
        TLift,
        TOp1,
        TOp2,
        TRef
    ),
    Value (VBool, VNum, VFunc),
    asFunction,
    eval,
    trace
) where
    import qualified Debug.Trace as T

    type Environment = [(String, Value)]

    envLookup :: Environment -> String -> Value
    envLookup []            s = error $ "Variable '" ++ s ++ "' not found"
    envLookup ((nn, nv):ns) s = if nn == s then nv else envLookup ns s

    data Expression
        = EApply Expression Expression
        | EIf Expression Expression Expression
        | EInput Environment Expression
        | ELambda String Expression
        | ELet String Expression Expression
        | ELift Value
        | EOp1 Op1 Expression
        | EOp2 Op2 Expression Expression
        | ERef String
        deriving (Show)

    data Op1 = Neg | Sin
        deriving (Show)
    data Op2 = Add | Equ | Gt | Gte | Lt | Lte | Mul | Neq | Sub
        deriving (Show)

    data Trace
        = TLift Value
        | TOp1 Op1 Value
        | TOp2 Op2 Value Value
        | TRef String
        deriving (Show)

    data Value = VBool Bool | VNum Float | VFunc (Value -> Value) | TFunc (Value -> [Trace] -> (Value, [Trace]))
    
    instance Show Value where
        show :: Value -> String
        show (VBool b) = "(VBool " ++ show b ++ ")"
        show (VNum  v) = "(VNum " ++ show v ++ ")"
        show (VFunc _) = "VFunc"
        show (TFunc _) = "TFunc"

    asFunction :: Expression -> Environment -> Expression
    asFunction e1 n = EInput n e1

    eval :: Environment -> Expression -> Value
    eval n (EApply  ef e1)    =
        let ef' = eval n ef
            e1' = eval n e1
        in
            case ef' of
                VFunc f -> f e1'
                _       -> error "ef' is not a function in eval/EApply"
    eval n (EIf     eb e1 e2) =
        case eval n eb of
            VBool True  -> eval n e1
            VBool False -> eval n e2
            _           -> error "eb does not resolve to bool in eval/EIf"
    eval _ (EInput  n  e1)    = eval n e1
    eval n (ELambda s  e1)    = VFunc (\x -> eval ((s, x) : n) e1)
    eval n (ELet    s  e1 e2) = eval ((s, eval n e1) : n) e2
    eval _ (ELift   v)        = v
    eval n (EOp1    op e1)    =
        case op of
            Neg -> case eval n e1 of
                VBool b -> VBool $ not b
                _       -> error "e1 does not resolve to bool in eval/EOp1/Neg"
            Sin -> case eval n e1 of
                VNum f -> VNum $ sin f
                _      -> error "e1 does not resolve to float in eval/EOp1/Sin"
    eval n (EOp2    op e1 e2) =
        let e1' = eval n e1
            e2' = eval n e2
        in
            case op of
                Add -> case (e1', e2') of
                    (VNum v1, VNum v2) -> VNum $ v1 + v2
                    _                  -> error "e1' or e2' is not a float in eval/EOp2/Add"
                Equ -> case (e1', e2') of
                    (VBool v1, VBool v2) -> VBool $ v1 == v2
                    (VNum  v1, VNum  v2) -> VBool $ v1 == v2
                    _                    -> error "e1' or e2' is not a bool or a float in eval/EOp2/Equ"
                Gt  -> case (e1', e2') of
                    (VNum v1, VNum v2) -> VBool $ v1 > v2
                    _                  -> error "e1' or e2' is not a float in eval/EOp2/Gt"
                Gte -> case (e1', e2') of
                    (VNum v1, VNum v2) -> VBool $ v1 >= v2
                    _                  -> error "e1' or e2' is not a float in eval/EOp2/Gte"
                Lt  -> case (e1', e2') of
                    (VNum v1, VNum v2) -> VBool $ v1 < v2
                    _                  -> error "e1' or e2' is not a float in eval/EOp2/Lt"
                Lte -> case (e1', e2') of
                    (VNum v1, VNum v2) -> VBool $ v1 <= v2
                    _                  -> error "e1' or e2' is not a float in eval/EOp2/Lte"
                Mul -> case (e1', e2') of
                    (VNum v1, VNum v2) -> VNum $ v1 * v2
                    _                  -> error "e1' or e2' is not a float in eval/EOp2/Mul"
                Neq -> case (e1', e2') of
                    (VBool v1, VBool v2) -> VBool $ v1 /= v2
                    (VNum  v1, VNum  v2) -> VBool $ v1 /= v2
                    _                    -> error "e1' or e2' is not a bool or a float in eval/EOp2/Neq"
                Sub -> case (e1', e2') of
                    (VNum v1, VNum v2) -> VNum $ v1 - v2
                    _                  -> error "e1' or e2' is not a float in eval/EOp2/Sub"
    eval n (ERef    rf)       = envLookup n rf

    trace :: Environment -> Expression -> [Trace] -> (Value, [Trace])
    trace n (EApply  ef e1)    t =
        let (v1, t1) = trace n e1 t
            (vf, tf) = trace n ef t1
        in
            case vf of
                TFunc f -> f v1 tf
                _       -> error "Type mismatch in trace/EApply"
    trace n (EIf     eb e1 e2) t =
        let (vb, tb) = trace n eb t
        in  case vb of
            VBool b -> if b then trace n e1 tb else trace n e2 tb
            _       -> error "Type mismatch in trace/EIf"
    trace _ (EInput  n  e1)    _ = trace n e1 []
    trace n (ELambda s  ef)    t = (TFunc $ \x t' -> trace ((s, x) : n) ef t', t)
    trace n (ELet    s  e1 e2) t =
        let (v1, t1) = trace n e1 t
        in  trace ((s, v1) : n) e2 t1
    trace _ (ELift   v)        t = (v, t)
    trace n (EOp1    op e1)    t =
        let (v1, t1) = trace n e1 t
        in  case (op, v1) of
            (Neg, VBool a) -> (VBool $ not a, TOp1 Neg v1 : t1)
            (Neg, _)       -> error "Type mismatch in trace/EOp1/Neg"
            (Sin, VNum  a) -> (VNum  $ sin a, TOp1 Sin v1 : t1)
            (Sin, _)       -> error "Type mismatch in trace/EOp1/Sin"
    trace n (EOp2    op e1 e2) t =
        let (v1, t1) = trace n e1 t
            (v2, t2) = trace n e2 t1
        in case (op, v1, v2) of
            (Add, VNum  a, VNum  b) -> (VNum  $ a +  b, TOp2 Add v1 v2 : t2)
            (Add, _,       _)       -> error "Type mismatch in trace/EOp2/Add"
            (Equ, VBool a, VBool b) -> (VBool $ a == b, TOp2 Equ v1 v2 : t2)
            (Equ, VNum  a, VNum  b) -> (VBool $ a == b, TOp2 Equ v1 v2 : t2)
            (Equ, _,       _)       -> error "Type mismatch in trace/EOp2/Equ"
            (Gt,  VNum  a, VNum  b) -> (VBool $ a >  b, TOp2 Gt  v1 v2 : t2)
            (Gt,  _,       _)       -> error "Type mismatch in trace/EOp2/Gt"
            (Gte, VNum  a, VNum  b) -> (VBool $ a >= b, TOp2 Gte v1 v2 : t2)
            (Gte, _,       _)       -> error "Type mismatch in trace/EOp2/Gte"
            (Lt,  VNum  a, VNum  b) -> (VBool $ a <  b, TOp2 Lt  v1 v2 : t2)
            (Lt,  _,       _)       -> error "Type mismatch in trace/EOp2/Lt"
            (Lte, VNum  a, VNum  b) -> (VBool $ a <= b, TOp2 Lte v1 v2 : t2)
            (Lte, _,       _)       -> error "Type mismatch in trace/EOp2/Lte"
            (Mul, VNum  a, VNum  b) -> (VNum  $ a *  b, TOp2 Mul v1 v2 : t2)
            (Mul, _,       _)       -> error "Type mismatch in trace/EOp2/Mul"
            (Neq, VBool a, VBool b) -> (VBool $ a /= b, TOp2 Neq v1 v2 : t2)
            (Neq, VNum  a, VNum  b) -> (VBool $ a /= b, TOp2 Neq v1 v2 : t2)
            (Neq, _,       _)       -> error "Type mismatch in trace/EOp2/Neq"
            (Sub, VNum  a, VNum  b) -> (VNum  $ a -  b, TOp2 Sub v1 v2 : t2)
            (Sub, _,       _)       -> error "Type mismatch in trace/EOp2/Sub"
    trace n (ERef    rf)       t = (envLookup n rf, t)