{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
-- Let [("x", I 30)]
--   (Ite (Ref "x") _ _)


data Val env where
  Empty :: Val '[]
  Push :: Val env -> a -> Val (a ': env)

data Idx env a where
  Z :: Idx (a ': env) a
  S :: Idx env a -> Idx (b ': env) a

-- data Idx where
--   Z :: Idx
--   S :: Idx -> Idx

-- data Idx = Z | S Idx

-- S (S (S Z)) :: Idx (d : c : b : a : env) a

data Expr env a where
  V :: Idx env a -> Expr env a
  Lam :: Expr (a ': env) b -> Expr env (a -> b)
  Let :: Expr env a -> Expr (a ': env) b -> Expr env b
  Let' :: LeftHandSide env env'
       -> Expr env' b
       -> Expr env b
  App :: Expr env (a -> b) -> Expr env a -> Expr env b



-- Γ |- t : τ


