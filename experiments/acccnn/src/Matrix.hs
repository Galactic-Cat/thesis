{-# LANGUAGE ExplicitNamespaces #-}

module Matrix (mmm, mvm) where
    import Data.Array.Accelerate as A
    import Prelude as P ()

    mmm :: Acc (Matrix Float) -> Acc (Matrix Float) -> Acc (Matrix Float)
    mmm ma mb =
        let
            ca = replicate (I3 (constant All) (n :: Exp Int) (constant All)) ma             -- 3D Array where ca[row, column, *] => ma[row, *]
            cb = replicate (I3 (l :: Exp Int) (constant All) (constant All)) (transpose mb) -- 3D Array where cb[row, column, *] => mb[*, column]
        in
            fold (+) 0.0 $ zipWith (*) ca cb
        where
            (I2 l _) = shape ma
            (I2 _ n) = shape mb
            
    mvm :: Acc (Matrix Float) -> Acc (Vector Float) -> Acc (Matrix Float)
    mvm mat vec =
        let
            rvec = replicate (I2 r (constant All)) vec
        in
            zipWith (*) mat rvec
        where
            (I2 r _) = shape mat