module Main (main) where
    import Matrix (test, testma, testmb)

    main :: IO ()
    main = do
        print testma
        print testmb
        print test