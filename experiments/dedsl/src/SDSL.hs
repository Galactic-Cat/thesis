module SDSL (eval, Expression (Add, Let, Num, Mul, Ref)) where
    data Expression =
        Add Expression Expression |
        Let [(String, Expression)] Expression |
        Num Float |
        Mul Expression Expression |
        Ref String
        deriving (Show)

    eval :: Expression -> Float
    eval (Add e1  e2) = eval e1 + eval e2
    eval (Let ess ev) = case ev of
                            (Num v)      -> v
                            (Ref s)      -> eval $ Let ess (getVar s ess)
                            (Let oes eo) -> eval $ Let (ess ++ oes) eo
                            (Add e1  e2) -> eval $ Add (Let ess e1) (Let ess e2)
                            (Mul e1  e2) -> eval $ Mul (Let ess e1) (Let ess e2)
        where getVar :: String -> [(String, Expression)] -> Expression
              getVar _ []          = error "Namespace doesn't contain variable"
              getVar s ((n, e):es) = if s == n then e else getVar s es
    eval (Num v)      = v
    eval (Mul e1  e2) = eval e1 * eval e2
    eval (Ref _)      = error "Trying to evaluate Ref out of let binding"