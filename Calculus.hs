module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger x  = Val (fromInteger x)
  negate (Val 0) = Val 0
  negate (UnApp Neg ex) = ex
  negate ex      = UnApp Neg ex
  (Val 0) + ex   = ex
  ex + (Val 0)   = ex
  ex1 + ex2      = BinApp Add ex1 ex2
  ex * (Val 1)   = ex
  (Val 1) * ex   = ex
  ex * (Val 0)   = Val 0
  (Val 0) * ex   = Val 0
  ex1 * ex2      = BinApp Mul ex1 ex2
  signum         = undefined
  abs            = undefined

instance Fractional Exp where
  fromRational x = Val (fromRational x)
  (Val 0) / ex   = 0
  ex / (Val 1)   = ex
  ex1 / ex2      = BinApp Div ex1 ex2
  recip          = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------
unOps = [(Sin, (sin)), (Cos, (cos)), (Log, (log)), (Neg, (negate))]
binOps = [(Add, (+)), (Mul, (*)), (Div, (/))]
unOpStrings = [(Sin, "sin"), (Cos, "cos"), (Log, "log"), (Neg, "-")]
binOpStrings = [(Add, "+"), (Mul, "*"), (Div, "/")]

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key list = fromJust (lookup key list)

-- Presents an expression in a user-friendly way
showExp :: Exp -> String
showExp (Val number) = show number
showExp (Id string) = string
showExp (UnApp operator ex)
  = (lookUp operator unOpStrings) ++ "(" ++ showExp ex ++ ")"
showExp (BinApp operator ex1 ex2)
  = "(" ++ showExp ex1 ++ (lookUp operator binOpStrings) ++ showExp ex2 ++ ")"

-- Evaluates an expression
eval :: Exp -> Env -> Double
eval (Val number) env = number
eval (Id string) env  = lookUp string env
eval (UnApp operator ex) env
  = (lookUp operator unOps) (eval ex env)
eval (BinApp operator ex1 ex2) env
  = (lookUp operator binOps) (eval ex1 env) (eval ex2 env)

-- Differentiates an expression
diff :: Exp -> String -> Exp
diff (Val number) var = Val 0
diff (Id string) var
  | string == var = Val 1
  | otherwise            = Val 0
diff (UnApp operator ex) var
  | operator == Neg = UnApp Neg (diff ex var)
  | operator == Sin = BinApp Mul (UnApp Cos ex) (diff ex var)
  | operator == Cos = UnApp Neg (BinApp Mul (UnApp Sin ex) (diff ex var))
  | operator == Log = BinApp Div (diff ex var) ex
diff (BinApp operator ex1 ex2) var
  | operator == Add = BinApp Add (diff ex1 var) (diff ex2 var)
  | operator == Mul = BinApp Add (BinApp Mul ex1 (diff ex2 var)) (BinApp Mul (diff ex1 var) ex2)
  | operator == Div = BinApp Div (BinApp Add (BinApp Mul (diff ex1 var) ex2) (UnApp Neg (BinApp Mul ex1 (diff ex2 var)))) (BinApp Mul ex2 ex2)

--This is the improved version of diff
--It is used by maclaurin but I left the original diff
diff2 :: Exp -> String -> Exp
diff2 (Val number) var = Val 0
diff2 (Id string) var
  | string == var = Val 1
  | otherwise            = Val 0
diff2 (UnApp operator ex) var
  | operator == Neg = negate (diff2 ex var)
  | operator == Sin = (cos ex) * (diff2 ex var)
  | operator == Cos = negate ((sin ex) * (diff2 ex var))
  | operator == Log = (diff2 ex var) / ex
diff2 (BinApp operator ex1 ex2) var
  | operator == Add = (diff2 ex1 var) + (diff2 ex2 var)
  | operator == Mul = (ex1 * (diff2 ex2 var)) + ((diff2 ex1 var) * ex2)
  | operator == Div
    = (((diff2 ex1 var) * ex2) + (negate (ex1 * (diff2 ex2 var)))) / (ex2 * ex2)


maclaurin :: Exp -> Double -> Int -> Double
maclaurin ex point n = sum (take n (zipWith3 getValue derivatives powers facts))
  where
    facts       = scanl (*) 1 [1..]
    derivatives = map (flip eval [("x", 0)]) (iterate (flip diff2 "x") ex)
    powers      = iterate (*point) 1
    getValue derivative power factorial = derivative*power/factorial
---------------------------------------------------------------------------
--Test cases...
--The validity of the added test cases was checked using a derivative calculator
e1, e2, e3, e4, e5, e6, e7, e8, e9 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

-- log(x^4 + x^3)
e7 = log (((Id "x")*(Id "x")*(Id "x")*(Id "x")) + ((Id "x")*(Id "x")*(Id "x")))

-- sin(cos(x))
e8 = sin(cos(Id "x"))

-- log(sin(x)) + x^3
e9 = log(sin(Id "x"))+(Id "x")*(Id "x")*(Id "x")

-- sin(x^3) + x^2
e10 = sin((Id "x")*(Id "x")*(Id "x")) + (Id "x")*(Id "x")
