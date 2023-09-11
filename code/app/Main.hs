module Main (main) where
    import Expression (
        Expression (ELet, ELift, EOp3, ELambda, EOp2, ERef),
        Op2 (Mul),
        Op3 (Fold),
        EValue (EReal, EArray),
        EEnvironment,
        eval)
    import Trace (trace, TValue, Trace)
    import Forward (forward, FValue (FReal, FArray), Forward)
    import Reverse (reverse, Adjoint (AReal, AArray), Reverse)
    import qualified Data.Map.Strict as Map
    import Prelude hiding (reverse, exp)

    input :: EEnvironment
    input = Map.empty

    exp :: Expression
    exp =
        ELet "f"
                (ELambda "x"
                    (ELambda "y"
                        (EOp2 Mul (ERef "x") (ERef "y"))))
                (ELet "z"
                    (ELift (EReal 1.0))
                    (ELet "a"
                        (ELift (EArray [1.0, 2.0, 3.0, 4.0, 5.0]))
                        (EOp3 Fold (ERef "f") (ERef "a") (ERef "z"))))

    result :: EValue
    result = eval input exp

    tt :: (TValue, Trace)
    tt = trace input True exp

    ff :: (FValue, Forward)
    ff = forward input True exp
    
    rv :: Reverse
    rv = case fst ff of
         FReal  s _  -> reverse (snd ff) s (AReal 1.0)
         FArray s xs -> reverse (snd ff) s (AArray (replicate (length xs) 1.0))
         _           -> error "This shouldn't happen"

    main :: IO ()
    main = do
        putStrLn "\nEval result:"
        print result
        putStrLn "\nTrace result:"
        print (fst tt)
        putStrLn "\nTrace trace:"
        print (snd tt)
        putStrLn "\nForward result:"
        print (fst ff)
        putStrLn "\nForward trace:"
        print (snd ff)
        putStrLn "\nReverse pass:"
        print rv
