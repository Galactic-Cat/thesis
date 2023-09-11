module Expression (
    Expression (
        EApply,
        EIf,
        ELambda,
        ELet,
        ELift,
        EOp0,
        EOp1,
        EOp2,
        EOp3,
        ERef
    ),
    EValue (EArray, EBool, EReal),
    Op0 (Iota),
    Op1 (Gen, Idx, Neg, Sin, Sum),
    Op2 (Add, Equ, Gt, Gte, Lt, Lte, Map, Mul, Neq, Sub),
    Op3 (Fold),
    EEnvironment,
    eval
) where
    import qualified Data.Map.Strict as Map
    import Data.Map (Map ())

    data Expression
        = EApply  Expression Expression
        | EIf     Expression Expression Expression
        | ELambda String     Expression
        | ELet    String     Expression Expression
        | ELift   EValue
        | EOp0    Op0
        | EOp1    Op1        Expression
        | EOp2    Op2        Expression Expression
        | EOp3    Op3        Expression Expression Expression
        | ERef    String
        deriving (Show)

    data EValue = EArray [Float] | EBool Bool | EReal Float | EFunc (EValue -> EValue)
    
    instance Show EValue where
        show (EArray v) = show v
        show (EBool  v) = show v
        show (EReal  v) = show v
        show (EFunc  _) = "EFunc"
    
    newtype Op0 = Iota Int
        deriving (Show)
    data Op1 = Gen Int | Idx Int | Neg | Sin | Sum
        deriving (Show)
    data Op2 = Add | Equ | Gt | Gte | Lt | Lte | Map | Mul | Neq | Sub
        deriving (Show)
    data Op3 = Fold
        deriving (Show)

    type EEnvironment = Map String EValue

    eval :: EEnvironment -> Expression -> EValue
    eval n (EApply  e1 e2)    =
        let v1 = eval n e1
            v2 = eval n e2
        in  case v1 of
                EFunc f -> f v2
                _       -> error "Type mismatch in eval/EApply"
    eval n (EIf     e1 e2 e3) = 
        case eval n e1 of
            EBool True  -> eval n e2
            EBool False -> eval n e3
            _           -> error "Type mismatch in eval/EIf"
    eval n (ELambda s1 e1)    = EFunc $ \x -> eval (Map.insert s1 x n) e1
    eval n (ELet    s1 e1 e2) = eval (Map.insert s1 (eval n e1) n) e2
    eval _ (ELift   v1)       = v1
    eval _ (EOp0    (Iota a)) = EArray [0.0 .. (fromIntegral a - 1)]
    eval n (EOp1    op e1)    =
        let v1 = eval n e1
        in  case (op, v1) of
            (Gen i, EFunc  a) -> EArray $ mapOver [0 .. (fromIntegral i)]
                where
                    mapOver []     = []
                    mapOver (x:xs) = case a (EReal x) of
                        EReal v -> v : mapOver xs
                        _       -> error "Type mismatch in eval/EOp1 (1)"
            (Idx i, EArray a) -> EReal $ a !! i
            (Neg,   EBool  a) -> EBool $ not a
            (Sin,   EReal  a) -> EReal $ sin a
            (Sum,   EArray a) -> EReal $ sum a
            _                 -> error "Type mismatch in eval/EOp1 (2)"
    eval n (EOp2    op e1 e2) =
        let v1 = eval n e1
            v2 = eval n e2
        in  case (op, v1, v2) of
            (Add, EReal a, EReal  b) -> EReal  $ a +  b
            (Equ, EBool a, EBool  b) -> EBool  $ a == b
            (Equ, EReal a, EReal  b) -> EBool  $ a == b
            (Gt,  EReal a, EReal  b) -> EBool  $ a >  b
            (Gte, EReal a, EReal  b) -> EBool  $ a >= b
            (Lt,  EReal a, EReal  b) -> EBool  $ a <  b
            (Lte, EReal a, EReal  b) -> EBool  $ a <= b
            (Map, EFunc a, EArray b) -> EArray $ map (unliftFloat . a . EReal) b
            (Mul, EReal a, EReal  b) -> EReal  $ a *  b
            (Neq, EBool a, EBool  b) -> EBool  $ a /= b
            (Neq, EReal a, EReal  b) -> EBool  $ a /= b
            (Sub, EReal a, EReal  b) -> EReal  $ a -  b
            _                        -> error "Type mismatch in eval/EOp2"
    eval n (EOp3     Fold e1 e2 e3) =
        let v1 = eval n e1
            v2 = eval n e2
            v3 = eval n e3
        in  case (v1, v2, v3) of
            (EFunc a, EArray b, EReal c) -> evalFold a b c
            _                            -> error "Type mismatch in eval/EOp3"
    eval n (ERef     s1)       = n Map.! s1

    unliftFloat :: EValue -> Float
    unliftFloat (EReal v) = v
    unliftFloat _         = error "Type mismatch in unliftFloat"

    unliftFunc :: EValue -> (EValue -> EValue)
    unliftFunc (EFunc f) = f
    unliftFunc _         = error "Type mismatch in unliftFunc"

    evalFold :: (EValue -> EValue) -> [Float] -> Float -> EValue
    evalFold _ []     z = EReal z
    evalFold f (x:xs) z = evalFold f xs $ unliftFloat (unliftFunc (f (EReal x)) (EReal z))