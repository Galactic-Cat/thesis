{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Backward (lift, primal, dual, seed, Variable) where
    import Control.Concurrent (MVar)
    import Control.Concurrent.MVar (newMVar, takeMVar, readMVar, putMVar)
    import Data.Functor ((<&>))

    data VariableBase = Variable Float (MVar [Float])
    type Variable = IO VariableBase

    lift :: Float -> Variable
    lift v = newMVar [] <&> Variable v

    primal :: Variable -> IO Float
    primal iovar = do (Variable v _) <- iovar
                      return v

    dual :: Variable -> IO Float
    dual iovar = do (Variable _ ml) <- iovar
                    l <- readMVar ml
                    return $ sum l

    seed :: Variable -> Float -> IO ()
    seed iovar s = do (Variable _ ml) <- iovar
                      l <- takeMVar ml
                      if   null l
                      then putMVar ml (s:l)
                      else error "Fail"

    instance Num Variable where
        (+) :: Variable -> Variable -> Variable
        (+) iovara iovarb = do (Variable va mla) <- iovara
                               (Variable vb mlb) <- iovarb
                               la <- takeMVar mla
                               lb <- takeMVar mlb
                               mvard <- newMVar []
                               _ <- let f = do dc <- readMVar mvard
                                               return $ sum dc
                                    in do v <- f
                                          putMVar mla (v:la)
                                          putMVar mlb (v:lb)
                               return $ Variable (va + vb) mvard
        (*) :: Variable -> Variable -> Variable
        (*) iovara iovarb = do (Variable va mla) <- iovara
                               (Variable vb mlb) <- iovarb
                               la <- takeMVar mla
                               lb <- takeMVar mlb
                               mvard <- newMVar []
                               _ <- let fa = do dc <- readMVar mvard
                                                return $ sum dc * vb
                                    in do v <- fa
                                          putMVar mla (v:la)
                               _ <- let fb = do dc <- readMVar mvard
                                                return $ sum dc * va
                                    in do v <- fb
                                          putMVar mlb (v:lb)
                               return $ Variable (va * vb) mvard
        negate :: Variable -> Variable
        negate iovar = do (Variable v ml) <- iovar
                          l <- takeMVar ml
                          mvard <- newMVar []
                          _ <- let f = do dc <- readMVar mvard
                                          return $ sum dc
                               in do f' <- f
                                     putMVar ml (f':l)
                          return $ Variable (-v) mvard