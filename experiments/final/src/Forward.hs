module Forward () where

    -- MODULE IMPORTS --

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

    -- TYPE DEFINITIONS --

    data FValue
        = FArray String [Float]
        | FBool  String Bool
        | FReal  String Float
        | FFunc  Bool   (FValue -> Int -> Forward -> (FValue, Forward, Int))

    data Forwarded
        = FLift TValue                  -- Lifting primals
        | FOp0  Op0                     -- Nullary operators
        | FOp1  Op1       String        -- Unary operators
        | FOp2  Op2       String String -- Binary operators
        | FMap  [Forward] String        -- Map operators (suitable for task parallelism)
        | FMapV Forward   String        -- Vectorized map operators (suitable for data parallelism)

    -- Forward trace, where each item is represented by its intermediate value, its operation, and the number of times it is referenced
    type Forward = Map String (TValue, Forwarded, Int)

    type FEnvironment = Map String FValue

    -- FUNCTION DEFINITIONS --

    -- Wrapper function for forward trace
    -- Inputs: an environment (for input variables), a boolean of whether to keep arrays or not, and the expression to trace
    -- Returns: a tuple containing the final value and the forward-pass trace
    forward :: EEnvironment -> Bool -> Expression -> (FValue, Forward)
    forward n k e =
        let n' = switchEnv n
            f' = traceInputs n'
            (v, f, _) = forward' n' k 0 e f'
        in  (v, f)

    -- Main forward trace function
    -- Inputs: the current environment, keep arrays boolean, name counter, current expression, current forward trace
    -- Outputs: tuple containing, expression value, forward trace, and updated name counter
    forward' :: FEnvironment -> Bool -> Int -> Expression -> Forward -> (FValue, Forward, Int)
    forward' n k c (EApply e1 e2) f =
        let (v1, f1, c1) = forward' n k c  e1 f
            (v2, f2, c2) = forward' n k c1 e2 f2
        in  case v1 of
            FFunc _ f -> f v2 c2 f2
            _         -> error "Type mismatch in forward'/EApply"

    -- Switch the starting environment from regular values to forward-trace values
    switchEnv :: EEnvironment -> FEnvironment
    switchEnv = Map.mapWithKey switch
        where
            switch k (EArray v) = FArray k v
            switch k (EBool  v) = FBool  k v
            switch k (EReal  v) = FReal  k v
            switch _ _          = error "Function in starting environment"

    -- Creates a forward trace from the input values
    traceInputs :: FEnvironment -> Forward
    traceInputs = Map.map $ \v -> (v, FLift v, 0)