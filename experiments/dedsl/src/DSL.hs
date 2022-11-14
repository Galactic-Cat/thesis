{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module DSL (eval, Expression (Add, B, Eq, F, I, Ite, Let, Mul, Ref)) where
    data Expression a where
        Add :: (Num a) => Expression a -> Expression a -> Expression a
        B   :: Bool -> Expression Bool
        Eq  :: (Eq a) => Expression a -> Expression a -> Expression Bool    
        F   :: Float -> Expression Float
        I   :: Int -> Expression Int
        Ite :: Expression Bool -> Expression a -> Expression a -> Expression a
        Let :: [(String, Expression a)] -> Expression b -> Expression b -- TODO: Single let binding
        Mul :: (Num a) => Expression a -> Expression a -> Expression a
        Ref :: String -> Expression a

    instance Show (Expression a) where
        show :: Expression a -> String
        show (Add e1  e2)    = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
        show (B   v)         = "B " ++ show v
        show (Eq  e1  e2)    = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
        show (F   v)         = "F " ++ show v
        show (I   v)         = "I " ++ show v
        show (Ite b   e1 e2) = "(" ++ show b ++ " ? " ++ show e1 ++ " : " ++ show e2 ++ ")"
        show (Let dss e)     = "Let [" ++ showDefs dss ++ "] in " ++ show e
            where showDefs []           = ""
                  showDefs ((n, ce):[]) = n ++ " := " ++ show ce
                  showDefs ((n, ce):ds) = n ++ " := " ++ show ce ++ ", " ++ showDefs ds
        show (Mul e1  e2)    = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
        show (Ref s)         = "@" ++ s

    eval :: Expression a -> a
    eval (Add e1  e2)    = eval e1 + eval e2
    eval (B   b)         = b
    eval (Eq  e1  e2)    = eval e1 == eval e2
    eval (F   f)         = f
    eval (I   i)         = i
    eval (Ite b   e1 e2) = if eval b then eval e1 else eval e2
    eval (Let ess ev)    = case ev of
                               (Add e1  e2)    -> eval $ Add (Let ess e1) (Let ess e2)
                               (B   b)         -> b
                               (Eq  e1  e2)    -> eval $ Eq (Let ess e1) (Let ess e2)
                               (F   f)         -> f
                               (I   i)         -> i
                               (Ite b   e1 e2) -> eval $ Ite (Let ess b) (Let ess e1) (Let ess e2)
                               (Let oes oe)    -> eval $ Let (oes ++ ess) oe
                               (Mul e1  e2)    -> eval $ Mul (Let ess e1) (Let ess e2)
                               (Ref s)         -> eval $ Let ess (getVar s ess)
        where getVar :: String -> [(String, Expression a)] -> Expression a
              getVar _ []          = error "Namespace doesn't contain variable"
              getVar s ((n, e):es) = if s == n then e else getVar s es
    eval (Mul e1  e2)    = eval e1 * eval e2
    eval (Ref _)         = error "Trying to evaluate Ref out of let binding"