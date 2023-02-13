module Main (main) where
    import Unt (
        asFunction,
        eval,
        trace,
        Expression (ERef, ELet, EOp1, ELambda, EApply, EOp2),
        Idx (Z, S),
        Op1 (Sin),
        Op2 (Add),
        Trace,
        Value (VNum))

    a :: Float
    a = 3
    b :: Float
    b = 5

    eTest :: Expression
    eTest = asFunction
        (ELet
            (ELet
                (EOp1 Sin (ERef Z))                  -- env: [w, x, y]
                (ELambda                             -- env: [z, w, x, y]
                    (EOp2 Add (ERef (S Z)) (ERef Z))
                )
            )                                        -- env: [f, x, y]
            (EApply (ERef Z)
                (EOp2 Add (ERef (S Z)) (ERef (S (S Z))))
            )
        )
        [VNum a, VNum b]

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

    (tv, tt) = trace [] eTest

    main :: IO ()
    main = do
        print $ hTest a b
        print $ htTest a b
        print $ eval [] eTest
        print tv
        print tt