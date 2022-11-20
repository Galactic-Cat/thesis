module Main (main) where
    import RGADT (dual, Environment (NEmpty), Expression (Let, Res), Idx (Z, S), RHS (Add, Mul, Sin, Val), test)

    seed :: Float
    seed = 1.0

    main :: IO ()
    main = print $ dual NEmpty test seed []

    -- expected tape = 1.0 : 1.0 : 1.0 : x1 : cos x1 + x2 : []
    -- expected tape = [1.0, 1.0, 1.0, 5.0, 8.284]