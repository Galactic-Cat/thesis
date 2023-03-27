module Main (main) where
    import Expression (
        Expression (ELet, ELambda, EOp0, EOp1, EOp2, ERef),
        Op0 (Iota),
        Op1 (Sum),
        Op2 (Map, Mul),
        EValue (EFloat),
        EEnvironment,
        eval)
    import Trace (trace)
    import qualified Data.Map.Strict as Map

    input :: EEnvironment
    input = Map.singleton "x" (EFloat 2)

    test :: Expression
    test =
        ELet "f"
            (ELambda "z"
                (EOp2 Mul (ERef "z") (ERef "x")))
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
