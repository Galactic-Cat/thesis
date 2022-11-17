module Main (main) where
    import RADT (dual,
             Environment (NEmpty), 
             eval, 
             Expression (Add, Let, Mul, Val, Ref),
             Tape (TEmpty))

    test :: Expression
    test =
        Let
            (Val 3.0) -- 0
            (Let
                (Val 1.0) -- 1
                (Let
                    (Mul (Ref 0) (Ref 1)) -- 2
                    (Let
                        (Add (Ref 0) (Ref 2)) -- 3
                        (Ref 3))))

    seed :: Float
    seed = 1.0

    tape :: Tape
    tape = dual NEmpty TEmpty test seed

    main :: IO ()
    main = print tape
