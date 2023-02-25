{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Trace (trace) where
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())
    import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
    import Control.Monad.ST (runST, ST)

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
        deriving (Show)

    data TValue = TBool String Bool | TFloat String Float | TFunc (TValue -> (TValue, Trace))

    instance Show TValue where
        show (TBool  s _) = s
        show (TFloat s _) = s
        show (TFunc  _)   = "TFunc"

    type Trace = Map String Traced

    type TEnvironment = Map String TValue

    (<:>) :: ST s (TValue, Trace) -> Trace -> ST s (TValue, Trace)
    (<:>) st1 t2 = do
        (v1, t1) <- st1
        return (v1, Map.union t1 t2)
    (<:->) :: (TValue, Trace) -> Trace -> (TValue, Trace)
    (<:->) (v1, t1) t2 = (v1, Map.union t1 t2)

    switchEnv :: EEnvironment -> TEnvironment
    switchEnv = Map.map switch
        where
            switch (EBool  v) = TBool  "x" v
            switch (EFloat v) = TFloat "x" v
            switch _          = error "Can't switch EFunc to TFunc"
    
    trace :: EEnvironment -> Expression -> (TValue, Trace)
    trace n e = runST $ do
        idc <- newSTRef 0
        trace' (switchEnv n) idc e

    trace' :: TEnvironment -> STRef s Int -> Expression -> ST s (TValue, Trace)
    trace' n c (EApply e1 e2)  = do
        (v1, t1) <- trace' n c e1
        (v2, t2) <- trace' n c e2 <:> t1
        case v1 of
            TFunc f -> return $ f v2 <:-> t2
            _       -> error "Type mismatch in trace'/EApply"
    trace' n c (EIf e1 e2 e3)  =
        let (v1, t1) = trace' n c e1
        in  case v1 of
            TBool _ True  -> trace' n c e2 <:> t1
            TBool _ False -> trace' n c e3 <:> t1
            _             -> error "Type mismatch in trace'/EIf"
    trace' n c (ELambda s1 e1) = (TFunc $ \x -> trace' (Map.insert s1 x n) c e1, Map.empty)
    trace' n c (ELet s1 e1 e2) =
        let (v1, t1) = trace' n c e1
        in  trace' (Map.insert s1 v1 n) c e2 <:> t1
    trace' _ c (ELift v1)      =
        let s = getName c
        in  case v1 of
            (EBool v) -> (TBool s v, Map.singleton s (TLift (TBool s v)))


    getName :: STRef s Int -> ST s String
    getName c = do
        c' <= readSTRef c
        writeSTRef c (c' + 1)
        return $ "$" ++ show c'
