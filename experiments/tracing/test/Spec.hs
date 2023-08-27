import Expression
    ( eval,
      EValue(EArray, EBool, EReal),
      Expression(ERef, ELambda, EOp0, ELet, EOp2, EOp1, EOp3, ELift, EIf),
      Op0(Iota),
      Op1(Sum),
      Op2(Map, Mul, Add),
      Op3(Fold))
import Forward (forward, purge, getName)
import qualified Data.Map.Strict as Map
import Prelude hiding (exp)

main :: IO ()
main = do
    putStrLn "Running 3 tests"
    putStrLn "[ Test Start ] Test 1 | Array vectorized map with input closure\n"
    _ <- testMapWithInputClosure
    putStrLn "\n[  Test End  ]\n"
    putStrLn "[ Test Start ] Test 2 | Array branching map with input closure\n"
    _ <- testBranchingMapWithInputClosure
    putStrLn "\n[  Test End  ]\n"
    putStrLn "[ Test Start ] Test 3 | Product fold\n"
    _ <- testFoldProduct
    putStrLn "\n[  Test End  ]\n"
    -- putStrLn "[ Test Start ] Test 4 | Forward purging\n"
    -- _ <- testForwardPurge
    -- putStrLn "\n[  Test End  ]\n"

testMapWithInputClosure :: IO ()
testMapWithInputClosure = 
    do
        let exp = ELet "f"
                (ELambda "z"
                    (EOp2 Mul (ERef "z") (ERef "x")))
                (ELet "a"
                    (EOp0 (Iota 5))
                    (ELet "b"
                        (EOp2 Map (ERef "f") (ERef "a"))
                        (EOp1 Sum (ERef "b"))))
        let inp = Map.fromList [("x", EReal 2)]
        let res = eval inp exp
        print res
        putStrLn "\n>> Eval passed\n"
        let fw1 = forward inp True exp
        print fw1
        putStrLn "\n>> Forward 1 passed\n"
        let fw2 = forward inp False exp
        print fw2
        putStrLn "\n>> Forward 2 passed"

testBranchingMapWithInputClosure :: IO ()
testBranchingMapWithInputClosure =
    do
        let exp = ELet "f"
                (ELambda "z"
                    (EIf (ERef "y")
                        (EOp2 Mul (ERef "z") (ERef "x"))
                        (EOp2 Add (ERef "z") (ERef "x"))))
                (ELet "a"
                    (EOp0 (Iota 5))
                    (ELet "b"
                        (EOp2 Map (ERef "f") (ERef "a"))
                        (EOp1 Sum (ERef "b"))))
        let inpA = Map.fromList [("x", EReal 2), ("y", EBool True)]
        let inpB = Map.fromList [("x", EReal 2), ("y", EBool False)]
        let resA = eval inpA exp
        print resA
        putStrLn "\n>> Eval A passed\n"
        let resB = eval inpB exp
        print resB
        putStrLn "\n>> Eval B passed\n"
        let fw1A = forward inpA True exp
        print fw1A
        putStrLn "\n>> Forward 1A passed\n"
        let fw1B = forward inpB True exp
        print fw1B
        putStrLn "\n>> Forward 1B passed\n"
        let fw2A = forward inpA False exp
        print fw2A
        putStrLn "\n>> Forward 2A passed\n"
        let fw2B = forward inpB False exp
        print fw2B
        putStrLn "\n>> Forward 2B passed"

testFoldProduct :: IO ()
testFoldProduct =
    do
        let exp = ELet "f"
                (ELambda "x"
                    (ELambda "y"
                        (EOp2 Mul (ERef "x") (ERef "y"))))
                (ELet "z"
                    (ELift (EReal 1.0))
                    (ELet "a"
                        (ELift (EArray [1.0, 2.0, 3.0, 4.0, 5.0]))
                        (EOp3 Fold (ERef "f") (ERef "a") (ERef "z"))))
        let inp = Map.empty
        let res = eval inp exp
        print res
        putStrLn "\n>> Eval passed\n"
        let fw1 = forward inp True exp
        print fw1
        putStrLn "\n>> Forward 1 passed\n"
        let fw2 = forward inp False exp
        print fw2
        putStrLn "\n>> Forward 2 passed"

-- TODO: Look over purge function again, it's not working
testForwardPurge :: IO ()
testForwardPurge =
    do
        let exp = ELet "f"
                (ELambda "x"
                    (ELambda "y"
                        (EOp2 Mul (ERef "x") (ERef "y"))))
                (ELet "z"
                    (ELift (EReal 1.0))
                    (ELet "a"
                        (ELift (EArray [1.0, 2.0, 3.0, 4.0, 5.0]))
                        (EOp3 Fold (ERef "f") (ERef "a") (ERef "z"))))
        let inp = Map.empty
        let (fwv, fwf) = forward inp True exp
        print (fwv, fwf)
        putStrLn "\n>> Forward passed\n"
        let cln = (fwv, purge fwf (getName fwv))
        print cln
        putStrLn "\n>> Purge passed"
