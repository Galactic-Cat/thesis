module Main (main) where
    import Expression (
        Expression (ELet, EIf, ELambda, EOp0, EOp1, EOp2, ERef),
        Op0 (Iota),
        Op1 (Sum),
        Op2 (Add, Map, Mul),
        EValue (EReal, EBool),
        EEnvironment,
        eval)
    import Trace (trace)
    import qualified Data.Map.Strict as Map

    input :: EEnvironment
    input = Map.fromList [("x", EReal 2), ("y", EBool False)]

    test :: Expression
    test =
        ELet "f"
            (ELambda "z"
                (EIf
                    (ERef "y")
                    (EOp2 Mul (ERef "z") (ERef "x"))
                    (EOp2 Add (ERef "z") (ERef "x"))))
            (ELet "a"
                (EOp0 (Iota 5))
                (ELet "b"
                    (EOp2 Map (ERef "f") (ERef "a"))
                    (EOp1 Sum (ERef "b"))))

    result :: EValue
    result = eval input test

    (tv, tt) = trace input True test

    main :: IO ()
    main = do
        print result
        print tv
        print tt        
