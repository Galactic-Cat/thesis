import Prelude

newtype Dual a = Dual (a, a -> a)

lift :: a -> Dual a
lift a = Dual (a, \d -> a)

unlift :: Dual a -> a -> a
unlift (Dual _, da) d = da d

instance Num a => Num (Dual a) where
    Dual (a, da) + Dual (b, db) = Dual (a + b, \d -> (da d) + (db d))
    Dual (a, da) - Dual (b, db) = Dual (a - b, \d -> (da d) - (db d))
    Dual (a, da) * Dual (b, db) = Dual (a * b, \d -> a * (da d) + b * (db d))
    abs (Dual (a, da)) = Dual (abs a, \d -> abs (da d))
    negate (Dual (a, da)) = Dual (-1 * a, \d -> -1 * (da d))

main = print "Hello world"
