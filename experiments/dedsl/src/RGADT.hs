{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}

module RGADT (
    dual,
    Environment (NEmpty),
    eval,
    Expression (Let, Res),
    Idx (Z, S),
    RHS (Add, Mul, Sin, Val),
    test
) where
    data Expression env a where
        Let :: RHS env a     -> Expression (a ': env) b -> Expression env b
        Res :: Idx env Float -> Expression env Float
    
    data RHS env a where
        Add :: Idx env Float -> Idx env Float -> RHS env Float
        Mul :: Idx env Float -> Idx env Float -> RHS env Float
        -- Ref :: Idx env Float -> RHS env Float
        Sin :: Idx env Float -> RHS env Float
        Val :: Float         -> RHS env Float

    data Idx env a where
        Z :: Idx (a ': env) a
        S :: Idx env a -> Idx (b ': env) a

    data Environment env where
        NEmpty :: Environment '[]
        NCons  :: Environment env -> a -> Environment (a ': env)

    eval :: Environment env -> Expression env a -> a
    eval n (Let rh lh) = eval (NCons n (evalRH n rh)) lh
    eval n (Res idx)   = envLookup n idx

    evalRH :: Environment env -> RHS env a -> a
    evalRH n (Add r1 r2) = envLookup n r1 + envLookup n r2
    evalRH n (Mul r1 r2) = envLookup n r1 * envLookup n r2
    -- evalRH n (Ref r)     = envLookup n r
    evalRH n (Sin r)     = sin $ envLookup n r
    evalRH _ (Val v)     = v

    envLookup :: Environment env -> Idx env a -> a
    envLookup NEmpty       _      = error "Environment is empty"
    envLookup (NCons _  v) Z      = v
    envLookup (NCons ns _) (S rs) = envLookup ns rs

    test :: Expression env Float
    test =
        Let                                
            (Val 5.0)                      
            (Let                           
                (Val 8.0)                  
                (Let                       
                    (Mul Z (S Z))          
                    (Let                   
                        (Sin (S (S Z)))    
                        (Let               
                            (Add Z (S Z))  
                            (Res Z)))))    

    dual :: Environment env -> Expression env a -> Float -> [Float] -> [Float]
    dual n (Let rh lh) d r = case rh of
                                 Add r1 r2 -> merge rv $ merge (listWrite r r1 dv)  (listWrite r r2 dv)
                                 Mul r1 r2 -> merge rv $ merge (listWrite r r1 dv1) (listWrite r r2 dv2)
                                    where dv1 = dv * envLookup n r2
                                          dv2 = dv * envLookup n r1
                                 Sin r1    -> merge rv $ listWrite r r1 (dv * cos (envLookup n r1))
                                 Val _     -> rv
        where rhv = evalRH n rh
              rv  = dual (NCons n rhv) lh d (0.0 : r)
              dv  = rv !! (length rv - (length r + 1))
    dual _ (Res ri)    d r = listWrite r ri d

    listWrite :: [Float] -> Idx env a -> Float -> [Float]
    listWrite []     Z      v = [v]
    listWrite (_:ls) Z      v = v : ls
    listWrite []     (S rs) v = 0.0 : listWrite [] rs v
    listWrite (l:ls) (S rs) v = l : listWrite ls rs v

    merge :: [Float] -> [Float] -> [Float]
    merge ls1 []  = ls1
    merge []  ls2 = ls2
    merge ls1@(l1:lss1) ls2@(l2:lss2)
        | length ls1 > length ls2 = merge ls1         (0.0 : ls2)
        | length ls1 < length ls2 = merge (0.0 : ls1) ls2
        | otherwise               = l1 + l2 : merge lss1 lss2
    