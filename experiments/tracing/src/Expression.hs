module Expression (
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
        | EOp1    Op1        Expression
        | EOp2    Op2        Expression Expression
        | ERef    String
        deriving (Show)

    data EValue = EBool Bool | EFloat Float | EFunc (EValue -> EValue)
    
    instance Show EValue where
        show (EBool  v) = show v
        show (EFloat v) = show v
        show (EFunc  _) = "EFunc"
    
    data Op1 = Neg | Sin
        deriving (Show)
    data Op2 = Add | Equ | Gt | Gte | Lt | Lte | Mul | Neq | Sub
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
    eval n (EOp1    op e1)    =
        let v1 = eval n e1
        in  case (op, v1) of
            (Neg, EBool  a) -> EBool $ not a
            (Neg, _)        -> error "Type mismatch in eval/EOp1/Neg"
            (Sin, EFloat a) -> EFloat $ sin a
            (Sin, _)        -> error "Type mismatch in eval/EOp1/Sin"
    eval n (EOp2    op e1 e2) =
        let v1 = eval n e1
            v2 = eval n e2
        in  case (op, v1, v2) of
            (Add, EFloat a, EFloat b) -> EFloat $ a +  b
            (Equ, EBool  a, EBool  b) -> EBool  $ a == b
            (Equ, EFloat a, EFloat b) -> EBool  $ a == b
            (Gt,  EFloat a, EFloat b) -> EBool  $ a >  b
            (Gte, EFloat a, EFloat b) -> EBool  $ a >= b
            (Lt,  EFloat a, EFloat b) -> EBool  $ a <  b
            (Lte, EFloat a, EFloat b) -> EBool  $ a <= b
            (Mul, EFloat a, EFloat b) -> EFloat $ a *  b
            (Neq, EBool  a, EBool  b) -> EBool  $ a /= b
            (Neq, EFloat a, EFloat b) -> EBool  $ a /= b
            (Sub, EFloat a, EFloat b) -> EFloat $ a -  b
            (_,   _,        _)        -> error "Type mismatch in eval/EOp2"
    eval n (ERef     s1)       = n Map.! s1