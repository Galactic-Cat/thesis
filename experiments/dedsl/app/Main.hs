module Main (main) where
    import DSL (eval, Expression (Add, F, I, Let, Ref))

    test = Let [("cat", F 3.2), ("dog", I 2)] (Add (Ref "cat") (Ref "dog"))

    main :: IO ()
    main = eval test
