module Main (main) where
    import Unt (
        asFunction,
        eval,
        trace,
        Expression (ERef, ELet, EOp1, ELambda, EApply, EOp2),
        Op1 (Sin),
        Op2 (Add),
        Value (VNum))

    a :: Float
    a = 3
    b :: Float
    b = 5

    eTest :: Expression
    eTest = asFunction
        (ELet
            "f" (ELet
                "w" (EOp1 Sin (ERef "x"))
                (ELambda "z"
                    (EOp2 Add (ERef "w") (ERef "z"))
                )
            )
            (EApply (ERef "f")
                (EOp2 Add (ERef "x") (ERef "y"))
            )
        )
        [("x", VNum a), ("y", VNum b)]

    hTest :: Float -> Float -> Float
    hTest x y =
        let w = sin x
            f z = w + z
        in  f (x + y)

    htTest :: Float -> Float -> Float
    htTest x y =
        let w = sin x
        in  let r1 = x + y
            in  let r2 = r1 + w
                in  r2

    (tv, tt) = trace [] eTest []

    main :: IO ()
    main = do
        print $ hTest a b
        print $ htTest a b
        print $ eval [] eTest
        print tv
        print (head (tail tt))
        mapM_ print tt