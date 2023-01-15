module Main (main) where
    import Lang (foldTest, eval, Environment (NEmpty))

    main :: IO ()
    main = print $ eval NEmpty foldTest
