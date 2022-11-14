{-# LANGUAGE InstanceSigs #-}

module Forward (Variable, sLift, uLift, primal, dual) where
    newtype Variable = Variable (Float, Float)
        deriving (Show)

    instance Num Variable where
        (+) :: Variable -> Variable -> Variable
        (+) v1 v2 = Variable (primal v1 + primal v2,
                              dual v1 + dual v2)
        (*) :: Variable -> Variable -> Variable
        (*) v1 v2 = Variable (primal v1 * primal v2,
                              primal v1 * dual v2 + primal v2 * dual v1)
        fromInteger :: Integer -> Variable
        fromInteger = uLift . fromInteger
        negate :: Variable -> Variable
        negate v = Variable (negate $ primal v, negate $ dual v)
        abs :: Variable -> Variable
        abs v = Variable (abs $ primal v, abs $ dual v)
        signum :: Variable -> Variable
        signum v = Variable (signum $ primal v, signum $ dual v)

    instance Fractional Variable where
        (/) :: Variable -> Variable -> Variable
        (/) v1 v2 = Variable (primal v1 / primal v2,
                              (dual v1 * primal v2 - primal v1 * dual v2) / (primal v2 ** 2))
        fromRational :: Rational -> Variable
        fromRational = uLift . fromRational

    instance Floating Variable where
        pi :: Variable
        pi = uLift pi
        sin :: Variable -> Variable
        sin v = Variable (sin $ primal v, cos $ primal v)

    sLift :: Float -> Variable
    sLift f = Variable (f, 1)

    uLift :: Float -> Variable
    uLift f = Variable (f, 0)

    primal :: Variable -> Float
    primal (Variable (f, _)) = f

    dual :: Variable -> Float
    dual (Variable (_, d)) = d