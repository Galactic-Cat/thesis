{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns #-}

module Unt (
    Idx (Z, S),
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
        TLet,
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
    data Idx = Z | S Idx
        deriving (Show)

    type Environment = [Value]

    data Expression
        = EApply Expression Expression
        | EIf Expression Expression Expression
        | EInput [Value] Expression
        | ELambda Expression
        | ELet Expression Expression
        | ELift Value
        | EOp1 Op1 Expression
        | EOp2 Op2 Expression Expression
        | ERef Idx
        deriving (Show)

    data Op1 = Neg | Sin
        deriving (Show)
    data Op2 = Add | Equ | Gt | Gte | Lt | Lte | Mul | Neq | Sub
        deriving (Show)

    data Trace
        = TLambda Trace
        | TLet Trace Trace
        | TLift Value
        | TOp1 Op1 Trace
        | TOp2 Op2 Trace Trace
        | TRef Idx
        deriving (Show)

    data Value = VBool Bool | VNum Float | VFunc (Value -> Value)
    
    instance Show Value where
        show :: Value -> String
        show (VBool b) = "(VBool " ++ show b ++ ")"
        show (VNum  v) = "(VNum " ++ show v ++ ")"
        show (VFunc _) = "VFunc"

    asFunction :: Expression -> ([Value] -> Expression)
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
    eval n (ELambda e1)       = VFunc (\x -> eval (x : n) e1)
    eval n (ELet    e1 e2)    = eval (eval n e1 : n) e2
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
    eval n (ERef    rf)       =
        case rf of
            Z     -> head n
            S rfs -> eval (tail n) (ERef rfs)

    trace :: Environment -> Expression -> (Value, Trace)
    trace n (EApply  ef e1)    = (vf, TLet t1 tf)
        where
            (v1, t1) = trace n e1
            (vf, tf) = traceApply (v1 : n) ef
    trace n (EIf     eb e1 e2) = (v3, TLet tb t3)
        where
            (vb, tb) = trace n eb
            (v3, t3) = case vb of
                VBool True  -> trace n e1
                VBool False -> trace n e2
                _           -> error "vb not a bool in trace/EIf"
    trace _ (EInput  n  e1)    = trace n e1
    trace n (ELambda e1)       = trace n e1
    trace n (ELet    ed e1)    = (v1, TLet td t1)
        where
            (vd, td) = trace n ed
            (v1, t1) = trace (vd : n) e1
    trace _ (ELift   v)        = (v, TLift v)
    trace n (EOp1    op e1)    =
        case op of
            Neg -> case trace n e1 of
                (VBool v1', t1) -> (VBool $ not v1', TOp1 Neg t1)
                _               -> error "v1 is not a bool in trace/EOp1/Neg"
            Sin -> case trace n e1 of
                (VNum v1', t1) -> (VNum v1', TOp1 Sin t1)
                _              -> error "v1 is not a float in trace/EOp1/Sin"
    trace n (EOp2    op e1 e2) =
        let (v1, t1) = trace n e1
            (v2, t2) = trace n e2
        in
            case op of
                Add -> case (v1, v2) of
                    (VNum v1', VNum v2') -> (VNum $ v1' + v2', TOp2 Add t1 t2)
                    _                    -> error "v1 or v2 is not a float in trace/EOp2/Add"
                Equ -> case (v1, v2) of
                    (VBool v1', VBool v2') -> (VBool $ v1' == v2', TOp2 Equ t1 t2)
                    (VNum  v1', VNum  v2') -> (VBool $ v1' == v2', TOp2 Equ t1 t2)
                    _                      -> error "v1 or v2 is not a bool or float in trace/EOp2/Equ"
                Gt  -> case (v1, v2) of
                    (VNum v1', VNum v2') -> (VBool $ v1' > v2', TOp2 Gt t1 t2)
                    _                    -> error "v1 or v2 is not a float in trace/EOp2/Gt"
                Gte  -> case (v1, v2) of
                    (VNum v1', VNum v2') -> (VBool $ v1' >= v2', TOp2 Gte t1 t2)
                    _                    -> error "v1 or v2 is not a float in trace/EOp2/Gte"
                Lt  -> case (v1, v2) of
                    (VNum v1', VNum v2') -> (VBool $ v1' < v2', TOp2 Lt t1 t2)
                    _                    -> error "v1 or v2 is not a float in trace/EOp2/Lt"
                Lte  -> case (v1, v2) of
                    (VNum v1', VNum v2') -> (VBool $ v1' <= v2', TOp2 Lte t1 t2)
                    _                    -> error "v1 or v2 is not a float in trace/EOp2/Lte"
                Mul -> case (v1, v2) of
                    (VNum v1', VNum v2') -> (VNum $ v1' * v2', TOp2 Mul t1 t2)
                    _                    -> error "v1 or v2 is not a float in trace/EOp2/Mul"
                Neq -> case (v1, v2) of
                    (VBool v1', VBool v2') -> (VBool $ v1' /= v2', TOp2 Neq t1 t2)
                    (VNum  v1', VNum  v2') -> (VBool $ v1' /= v2', TOp2 Neq t1 t2)
                    _                      -> error "v1 or v2 is not a bool or float in trace/EOp2/Neq"
                Sub -> case (v1, v2) of
                    (VNum v1', VNum v2') -> (VNum $ v1' - v2', TOp2 Sub t1 t2)
                    _                    -> error "v1 or v2 is not a float in trace/EOp2/Sub"
    trace n (ERef    rf)       = (eval n (ERef rf), TRef rf)

    traceApply :: Environment -> Expression -> (Value, Trace)
    traceApply n e@(EApply {})  = traceApply n e
    traceApply n   (ELambda e1) = trace n e1
    traceApply n e              = trace n e