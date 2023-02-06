{-# LANGUAGE DataKinds, GADTs #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns #-}

module Tac () where
    data Environment env where
        NEmpty :: Environment '[]
        NCons  :: Environment env -> a -> Environment (a ': env)

    data Expression env a where
        EApply  :: Expression env (a -> b)                -> Expression env a        -> Expression env b
        EFix    :: Expression env (a -> a)                -> Expression env a
        EIf     :: Expression env Bool                    -> Expression env a        -> Expression env a -> Expression env a
        ELambda :: (Expression env a -> Expression env b) -> Expression env (a -> b)
        ELet    :: Expression env a                       -> Expression (a ': env) b -> Expression env b
        ELift   :: a                                      -> Expression env a
        EOp1    :: Op1 a b                                -> Expression env a        -> Expression env b
        EOp2    :: Op2 a b c                              -> Expression env a        -> Expression env b -> Expression env c
        ERef    :: Idx env a                              -> Expression env a

    data Idx env a where
        Z :: Idx (a ': env) a
        S :: Idx env a -> Idx (b ': env) a

    data Op1 a b where
        Neg ::                 Op1 Bool Bool
        Sin :: (Floating a) => Op1 a    a

    data Op2 a b c where
        Add :: (Num a) => Op2 a a a
        Equ :: (Eq a)  => Op2 a a Bool
        Gt  :: (Ord a) => Op2 a a Bool
        Gte :: (Ord a) => Op2 a a Bool
        Lt  :: (Ord a) => Op2 a a Bool
        Lte :: (Ord a) => Op2 a a Bool
        Mul :: (Num a) => Op2 a a a
        Neq :: (Eq a)  => Op2 a a Bool
        Sub :: (Num a) => Op2 a a a

    data Trace env a where
        TInput :: a           -> Int                -> Trace env a
        TLet   :: Trace env a -> Trace (a ': env) b -> Trace env b
        TLift  :: a           -> Trace env a
        TOp1   :: Op1 a b     -> Trace env a        -> Trace env b
        TOp2   :: Op2 a b c   -> Trace env a        -> Trace env b -> Trace env c
        TRef   :: Idx env a   -> Trace env a

    eeval :: Environment env -> Expression env a -> a
    eeval n (EApply  ef e1)    = eeval n ef $ eeval n e1
    eeval n (EFix    ef)       = eeval n ef $ eeval n (EFix ef)
    eeval n (EIf     eb e1 e2) = if eeval n eb then eeval n e1 else eeval n e2
    eeval n (ELambda ef)       = eeval n . ef . ELift
    eeval n (ELet    ed e1)    = eeval (NCons n (eeval n ed)) e1
    eeval _ (ELift   ev)       = ev
    eeval n (EOp1    op e1)    =
        case op of
            Neg -> not $ eeval n e1
            Sin -> sin $ eeval n e1
    eeval n (EOp2    op e1 e2) =
        case op of
            Add -> eeval n e1 +  eeval n e2
            Equ -> eeval n e1 == eeval n e2
            Gt  -> eeval n e1 >  eeval n e2
            Gte -> eeval n e1 >= eeval n e2
            Lt  -> eeval n e1 <  eeval n e2
            Lte -> eeval n e1 <= eeval n e2
            Mul -> eeval n e1 *  eeval n e2
            Neq -> eeval n e1 /= eeval n e2
            Sub -> eeval n e1 -  eeval n e2
    eeval n (ERef    rf)       = envLookup n rf

    envLookup :: Environment env -> Idx env a -> a
    envLookup NEmpty       _     = error "Environment is empty"
    envLookup (NCons _  e) Z     = e
    envLookup (NCons ns _) (S r) = envLookup ns r

    teval :: Environment env -> Trace env a -> a
    teval _ (TInput tv _)     = tv
    teval n (TLet   td t1)    = teval (NCons n (teval n td)) t1
    teval _ (TLift  tv)       = tv
    teval n (TOp1   op t1)    =
        case op of
            Neg -> not $ teval n t1
            Sin -> sin $ teval n t1
    teval n (TOp2   op t1 t2) = 
        case op of
            Add -> teval n t1 +  teval n t2
            Equ -> teval n t1 == teval n t2
            Gt  -> teval n t1 >  teval n t2
            Gte -> teval n t1 >= teval n t2
            Lt  -> teval n t1 <  teval n t2
            Lte -> teval n t1 <= teval n t2
            Mul -> teval n t1 *  teval n t2
            Neq -> teval n t1 /= teval n t2
            Sub -> teval n t1 -  teval n t2
    teval n (TRef   rf)       = envLookup n rf

    trace :: Environment env -> Expression env a -> Trace env a
    trace n (EApply  ef e1)    = TLift $ eeval n ef (eeval n e1)
    -- EFix
    trace n (EIf     eb e1 e2) = if eeval n eb then trace n e1 else trace n e2
    -- ELambda
    trace n (ELet    ed e1)    = TLet (trace n ed) (trace (NCons n $ eeval n ed) e1)
    trace _ (ELift   ev)       = TLift ev
    trace n (EOp1    op e1)    = TOp1 op (trace n e1)
    trace n (EOp2    op e1 e2) = TOp2 op (trace n e1) (trace n e2)
    trace n (ERef    rf)       = TLift $ envLookup n rf