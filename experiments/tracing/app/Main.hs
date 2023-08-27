module Main (main) where
    import Expression (
        Expression (ELet, ELambda, EOp0, EOp1, EOp2, ERef),
        Op0 (Iota),
        Op1 (Sum),
        Op2 (Map, Mul),
        EValue (EReal, EBool),
        EEnvironment,
        eval)
    import Trace (trace, evalTrace, TValue (TReal, TArray))
    import Forward (forward)
    -- import Reverse (reverse)
    import qualified Data.Map.Strict as Map
    import Prelude hiding (reverse)

    input :: EEnvironment
    input = Map.fromList [("x", EReal 2), ("y", EBool True)]

    test :: Expression
    test =
        ELet "f"
            (ELambda "z"
                (EOp2 Mul (ERef "z") (ERef "x")))
                -- (EIf
                --     (ERef "y")
                --     (EOp2 Mul (ERef "z") (ERef "x"))
                --     (EOp2 Add (ERef "z") (ERef "x"))))
            (ELet "a"
                (EOp0 (Iota 5))
                (ELet "b"
                    (EOp2 Map (ERef "f") (ERef "a"))
                    (EOp1 Sum (ERef "b"))))

    result :: EValue
    result = eval input test

    (tv, tt) = trace input True test
    te = case tv of
        TReal  s _ -> evalTrace tt s
        TArray s _ -> evalTrace tt s
        _          -> error "nop"

    (fv, ft) = forward input True test
    -- rv = case tv of
    --     TReal  s _ -> reverse fw s 1.0
    --     TArray s _ -> reverse fw s 1.0

    main :: IO ()
    main = do
        -- print result
        print (tv, fv)
        print tt
        print ft
        -- print te
        -- print rv
