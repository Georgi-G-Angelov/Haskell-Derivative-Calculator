module Tests where

import IC.TestSuite hiding (Id)
import qualified IC.TestSuite as TS
import Calculus

evalTests
  = [ ((Val 7),  [("x",380)])
        ==> 7.0

    , ((Id "a"), [("x",380), ("a",42), ("t",10)])
        ==> 42.0

    , ( (BinApp Add (Val (-5)) (Id "t'")), [("t",10), ("t'",18)])
        ==> 13.0

    , ((UnApp Neg (BinApp Add (Val (-5)) (Id "t'"))), [("t",10), ("t'",19)])
        ==> (-14.0)

    , ((BinApp Mul (Id "x") (Id "x")), [("t",10), ("t'",18.6), ("x",-55)])
        ==> 3025.0

    , ((BinApp Div (Val 3) (Id "z")), [("z",7)])
        ==> 0.42857142857142855

    , ((UnApp Neg (Id "x")), [("x",0.37)])
        ==> (-0.37)

    , ((UnApp Sin (Val 2.4)), [])
        ==> 0.675463180551151

    , ((UnApp Cos (Val 2.4)), [])
        ==> (-0.7373937155412454)

    , ( e1, [("x",0.37)])
        ==> (1.85)

    , ( e2, [("x",0.37), ("y", 8.2)])
        ==> 1.3369

    , ( e3, [("x",0.37), ("y", 2.0)])
        ==> 4.216153846153846

    , ( e4, [("x",0.37)])
        ==> (-0.9323273456060345)

    , ( e5, [("x",0.37)])
        ==> 0.6433720724587564

    , ( e6, [("x",0.37)])
        ==> 0.8799171617597958
    ]

diffTests
  = [ (e1, "x") ==>
        BinApp Add (BinApp Mul (Val 5.0) (Val 1.0)) (BinApp Mul (Val 0.0)
                                                                (Id "x"))

    , (e2, "x") ==>
        BinApp Add (BinApp Add (BinApp Add (BinApp Mul (Id "x") (Val 1.0))
                               (BinApp Mul (Val 1.0) (Id "x"))) (Val 0.0))
                   (UnApp Neg (Val 0.0))

    , (e2, "y") ==>
        BinApp Add (BinApp Add (BinApp Add (BinApp Mul (Id "x") (Val 0.0))
                               (BinApp Mul (Val 0.0) (Id "x"))) (Val 1.0))
                   (UnApp Neg (Val 0.0))
    , (e4, "x") ==>
        UnApp Neg (UnApp Neg (BinApp Mul (UnApp Sin (Id "x")) (Val 1.0)))

    , (e5, "x") ==>
        BinApp Mul (UnApp Cos (BinApp Add (Val 1.0)
                                          (UnApp Log (BinApp Mul (Val 2.0)
                                                                 (Id "x")))))
                   (BinApp Add (Val 0.0)
                               (BinApp Div (BinApp Add (BinApp Mul (Val 2.0)
                                                                   (Val 1.0))
                                            (BinApp Mul (Val 0.0) (Id "x")))
                           (BinApp Mul (Val 2.0) (Id "x"))))
    , (e6, "x") ==>
        BinApp Div (BinApp Add (BinApp Add (BinApp Mul (Val 3.0)
                                           (BinApp Add (BinApp Mul (Id "x")
                                                                   (Val 1.0))
                                                       (BinApp Mul (Val 1.0)
                                                                   (Id "x"))))
                                       (BinApp Mul (Val 0.0)
                                                   (BinApp Mul (Id "x")
                                                               (Id "x"))))
                                (Val 0.0))
                    (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x")
                                                                  (Id "x")))
                                (Val 2.0))

    , (e7, "x") ==>
      BinApp Div (BinApp Add (BinApp Add (BinApp Mul (BinApp Mul
      (BinApp Mul (Id "x") (Id "x")) (Id "x")) (Val 1.0))
      (BinApp Mul (BinApp Add (BinApp Mul (BinApp Mul (Id "x") (Id "x"))
      (Val 1.0)) (BinApp Mul (BinApp Add (BinApp Mul (Id "x") (Val 1.0))
      (BinApp Mul (Val 1.0) (Id "x"))) (Id "x"))) (Id "x")))
      (BinApp Add (BinApp Mul (BinApp Mul (Id "x") (Id "x")) (Val 1.0))
      (BinApp Mul (BinApp Add (BinApp Mul (Id "x") (Val 1.0))
      (BinApp Mul (Val 1.0) (Id "x"))) (Id "x"))))
      (BinApp Add (BinApp Mul (BinApp Mul
      (BinApp Mul (Id "x") (Id "x")) (Id "x")) (Id "x"))
      (BinApp Mul (BinApp Mul (Id "x") (Id "x")) (Id "x")))

    , (e8, "x") ==>
      BinApp Mul (UnApp Cos (UnApp Cos (Id "x")))
      (UnApp Neg (BinApp Mul (UnApp Sin (Id "x")) (Val 1.0)))

    , (e9, "x") ==>
      BinApp Add (BinApp Div (BinApp Mul (UnApp Cos (Id "x")) (Val 1.0))
      (UnApp Sin (Id "x"))) (BinApp Add (BinApp Mul (BinApp Mul (Id "x")
      (Id "x")) (Val 1.0)) (BinApp Mul (BinApp Add (BinApp Mul (Id "x")
      (Val 1.0)) (BinApp Mul (Val 1.0) (Id "x"))) (Id "x")))

    , (e10, "x") ==>
      BinApp Add (BinApp Mul (UnApp Cos (BinApp Mul (BinApp Mul (Id "x")
      (Id "x")) (Id "x"))) (BinApp Add (BinApp Mul (BinApp Mul (Id "x")
      (Id "x")) (Val 1.0)) (BinApp Mul (BinApp Add (BinApp Mul (Id "x")
      (Val 1.0)) (BinApp Mul (Val 1.0) (Id "x"))) (Id "x"))))
      (BinApp Add (BinApp Mul (Id "x") (Val 1.0)) (BinApp Mul (Val 1.0)
      (Id "x")))
    ]

diff2Tests
  = [ (e1, "x") ==>
        Val 5.0
    , (e2, "x") ==>
        BinApp Add (Id "x") (Id "x")
    , (e2, "y") ==>
        Val 1.0
    , (e4, "x") ==>
        UnApp Sin (Id "x")
    , (e5, "x") ==>
        BinApp Mul (UnApp Cos (BinApp Add (Val 1.0)
        (UnApp Log (BinApp Mul (Val 2.0) (Id "x")))))
        (BinApp Div (Val 2.0) (BinApp Mul (Val 2.0) (Id "x")))
    , (e6, "x") ==>
        BinApp Div (BinApp Mul (Val 3.0) (BinApp Add (Id "x") (Id "x")))
        (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
        (Val 2.0))
    , (e7, "x") ==>
      BinApp Div (BinApp Add (BinApp Add (BinApp Mul (BinApp Mul (Id "x")
      (Id "x")) (Id "x")) (BinApp Mul (BinApp Add (BinApp Mul (Id "x") (Id "x"))
      (BinApp Mul (BinApp Add (Id "x") (Id "x")) (Id "x"))) (Id "x")))
      (BinApp Add (BinApp Mul (Id "x") (Id "x")) (BinApp Mul
      (BinApp Add (Id "x") (Id "x")) (Id "x")))) (BinApp Add
      (BinApp Mul (BinApp Mul (BinApp Mul (Id "x") (Id "x"))
      (Id "x")) (Id "x")) (BinApp Mul (BinApp Mul (Id "x") (Id "x")) (Id "x")))

    , (e8, "x") ==>
      BinApp Mul (UnApp Cos (UnApp Cos (Id "x")))
      (UnApp Neg (UnApp Sin (Id "x")))

    , (e9, "x") ==>
      BinApp Add (BinApp Div (UnApp Cos (Id "x")) (UnApp Sin (Id "x")))
      (BinApp Add (BinApp Mul (Id "x") (Id "x"))
      (BinApp Mul (BinApp Add (Id "x") (Id "x")) (Id "x")))

    , (e10, "x") ==>
      BinApp Add (BinApp Mul (UnApp Cos (BinApp Mul (BinApp Mul (Id "x")
      (Id "x")) (Id "x"))) (BinApp Add (BinApp Mul (Id "x") (Id "x"))
      (BinApp Mul (BinApp Add (Id "x") (Id "x")) (Id "x"))))
      (BinApp Add (Id "x") (Id "x"))
    ]

maclaurinTests
  = [ (UnApp Sin (Id "x"), 2, 2) ==> 2.0
    , (UnApp Sin (Id "x"), 2, 3) ==> 2.0
    , (UnApp Sin (Id "x"), 2, 5) ==> 0.6666666666666667
    , (UnApp Sin (Id "x"), 2, 7) ==> 0.9333333333333333
    , (UnApp Sin (Id "x"), 2, 9) ==> 0.9079365079365079
    , (UnApp Cos (Id "x"), 4, 9)  ==> (-0.39682539682539764)
    , (UnApp Cos (Id "x"), 3.14, 30) ==> (-0.9999987317275395)
    , (UnApp Cos (Id "x"), 3.1415, 100) ==> (-0.9999999957076559)
    , (UnApp Cos (Id "x"), 3.141592, 100) ==> (-0.999999999999786)
    , (UnApp Cos (Id "x"), 3.14159265359, 100) ==> (-1.0000000000000002)
    ]

showExpTests
  = [ (diff2 e1 "x") ==> "5.0"
    , (diff2 e2 "x") ==> "(x+x)"
    , (diff2 e2 "y") ==> "1.0"
    , (diff2 e4 "x") ==> "sin(x)"
    , (diff2 e5 "x") ==> "(cos((1.0+log((2.0*x))))*(2.0/(2.0*x)))"
    , (diff2 e6 "x") ==> "((3.0*(x+x))/((3.0*(x*x))+2.0))"
    , (diff2 e7 "x")
      ==> "(((((x*x)*x)+(((x*x)+((x+x)*x))*x))+((x*x)+((x+x)*x)))/((((x*x)*x)*x)+((x*x)*x)))"
    , (diff2 e8 "x") ==> "(cos(cos(x))*-(sin(x)))"
    , (diff2 e9 "x") ==> "((cos(x)/sin(x))+((x*x)+((x+x)*x)))"
    , (diff2 e10 "x") ==> "((cos(((x*x)*x))*((x*x)+((x+x)*x)))+(x+x))"
    ]

allTestCases
  = [ floatTestCase "eval"       (uncurry eval)       evalTests
    , testCase "diff"       (uncurry diff)       diffTests
    , testCase "diff2"      (uncurry diff2)      diff2Tests
    , floatTestCase "maclaurin"  (uncurry3 maclaurin) maclaurinTests
    , testCase "showExp" (showExp) showExpTests
    ]

runTests = mapM_ goTest allTestCases

main = runTests
