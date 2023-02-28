module Main (main) where
    import Expression (
        Expression (EApply, ELet, ELambda, EOp1, EOp2, ERef),
        Op1 (Sin),
        Op2 (Add),
        EValue (EFloat),
        EEnvironment,
        eval)
    import Trace (trace)
    import qualified Data.Map.Strict as Map

    input :: EEnvironment
    input = Map.insert "y" (EFloat 3) (Map.singleton "x" (EFloat 2))

    test :: Expression
    test = ELet "f"
        (ELet "w"
            (EOp1 Sin (ERef "x"))
            (ELambda "z" (EOp2 Add (ERef "w") (ERef "z"))))
        (EApply (ERef "f") (EOp2 Add (ERef "x") (ERef "y")))

    result :: EValue
    result = eval input test

    (tv, tt) = trace input test

    main :: IO ()
    main = do
        print result
        print tv
        print tt        
