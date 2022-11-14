module Main (main) where
    import Backward (Variable, lift, dual)

    main :: IO ()
    main = do seed y
              dual y >>= print

    x1 :: Variable
    x1 = lift 7
    x2 :: Variable
    x2 = lift 9

    y :: Variable
    y = x1 + x2
