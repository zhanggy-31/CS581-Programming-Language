module HW6 where

import Prelude hiding (print,and,or,not,pred,succ,fst,snd,either,length,sum,product)

import DeBruijn


--
-- * Part 1: Nameless lambda calculus
--

-- | λx. (λx.x) x
--
--   >>> eval ex1
--   Abs (Ref 0)
--
ex1 :: Exp
ex1 = Abs(App(Abs(Ref 0))(Ref 0))

-- | (λxy.xz) z z
--
--   >>> eval ex2
--   App (Ref 0) (Ref 0)
--
ex2 :: Exp
ex2 = App(App(abs2(App(Ref 1)(Ref 2)))(Ref 0))(Ref 0)


-- | λy. (λxy.yx) y z
--
--   >>> eval ex3
--   Abs (App (Ref 1) (Ref 0))
--   
ex3 :: Exp
ex3 = Abs(app2(abs2(App(Ref 0)(Ref 1)))(Ref 0)(Ref 1))


-- | Is the given nameless lambda calculus term a closed expression? That is,
--   does it contain no free variables?
--
--   >>> closed (Ref 0)
--   False
--
--   >>> closed (Abs (Ref 0))
--   True
--
--   >>> closed (Abs (App (Ref 0) (Ref 1)))
--   False
--
--   >>> closed (Abs (App (Abs (App (Ref 0) (Ref 1))) (Ref 0)))
--   True
--
closed :: Exp -> Bool
closed (Ref x) = False
closed (App (Ref x) (Ref y)) = False
closed (Abs (Ref x)) = if (x == 0) then True else False
closed (Abs (App (Ref x) (Ref y))) = if ((x == 0)&&(y == 0)) then True else False 

-- if Exp is (Ref x), it's not closed
-- if Exp is (App (Ref x) (Ref y)), it's not closed
-- if Exp is (Abs (Ref x)), it depends on x, if x equals 0, it's true, otherwise, it's false
-- however, case Abs is complex, it needs to count how far the Var is away from lambda, if
-- we can use eval to get the reduction of each Exp, it is solvable to me.
-- 
-- For example, closed(eval(ex1)) is True
-- closed(eval(ex2)) is False
-- closed(eval(ex3)) is False
-- closed(eval(Abs (App (Abs (App (Ref 0) (Ref 1))) (Ref 0)))) is True
--
--
--
--
-- * Part 2: Church pair update functions
--

-- | Write a lambda calculus function that replaces the first element in a
--   Church-encoded pair. The first argument to the function is the new
--   first element, the second argument is the original pair.
--
--   >>> :{ 
--     eval (app2 pair true (num 3)) == eval (app2 setFst true (app2 pair (num 2) (num 3)))
--   :}
--   True
--
setFst :: Exp
setFst = abs2 (app2 pair (Ref 1)(App snd (Ref 0)))


-- | Write a lambda calculus function that replaces the second element in a
--   Church-encoded pair. The first argument to the function is the new
--   second element, the second argument is the original pair.
--
--   >>> :{
--     eval (app2 pair (num 2) true) == eval (app2 setSnd true (app2 pair (num 2) (num 3)))
--   :}
--   True
--
setSnd :: Exp
setSnd = abs2 (app2 pair (App fst (Ref 0))(Ref 1))


--
-- * Part 3: Church encoding a Haskell program
--

-- | Pretend Haskell's Int is restricted to Nats.
type Nat = Int

-- | A simple data for representing shapes.
data Shape
   = Circle    Nat
   | Rectangle Nat Nat
  deriving (Eq,Show)

-- | A smart constructor for building squares.
square :: Nat -> Shape
square l = Rectangle l l

-- | Compute the area of a shape using a rough approximation of pi.
area :: Shape -> Nat
area (Circle r)      = 3 * r * r
area (Rectangle l w) = l * w

-- | Compute the perimeter of a shape using a rough approximation of pi.
perimeter :: Shape -> Nat
perimeter (Circle r)      = 2 * 3 * r
perimeter (Rectangle l w) = 2 * l + 2 * w

-- | Encode the circle constructor as a lambda calculus term. The term
--   should be a function that takes a Church-encoded natural number as input
--   and produces a Church-encoded shape as output.
circleExp :: Exp
circleExp = Abs(Ref 0)

-- For circleExp, it should get the radius of the circle.

-- | Encode the rectangle constructor as a lambda calculus term. The term
--   should be a function that takes two Church-encoded natural numbers as
--   input and produces a Church-encoded shape as output.
rectangleExp :: Exp
rectangleExp = Abs(App(Ref 0)(Ref 1))

-- For rectangleExp, it should get the length and wedth of the rectangle.

-- | Convert a shape into a lambda calculus term. This function helps to
--   illustrate how your encodings of the constructors should work.
encodeShape :: Shape -> Exp
encodeShape (Circle r)      = App circleExp (num r)
encodeShape (Rectangle l w) = app2 rectangleExp (num l) (num w)

-- | Encode the square function as a lambda calculus term.
squareExp :: Exp
squareExp = Abs(Ref 0)

-- For sqaureExp, it should get the number for square.

-- | Encode the area function as a lambda calculus term.
areaExp :: Exp
-- areaExp = Abs (app4 case3 
--                     (Abs(app3 mult (Ref 0)(Ref 0) three))
--                     (Abs(app2 mult (Ref 0)(Ref 1)))
--                     (Ref 0)
--                     (Ref 0))

-- For areaExp, it should have two cases, 
--1. for circle case, its area should be pi * r * r  
--2. for rectangle, its area should be w * l

-- | Encode the perimeter function as a lambda calculus term.
perimeterExp :: Exp
-- perimeterExp = Abs (app4 case3 
--                          (Abs(app3 mult (Ref 0) three two))
--                          (Abs(app2 mult two (app2 add (Ref 0) (Ref 1))))
--                          (Ref 0)
--                          (Ref 0))

-- For perimeterExp, it should have two cases, 
-- 1. for circle case, its perimeter should be 2 * pi * ridius
-- 2. for rectangle case, its perimeter should be 2 * (l + w) 


-- | Some tests of your lambda calculus encodings.
--
--   >>> :{
--     checkEval (area (Circle 3)) (App areaExp (App circleExp (num 3)))
--   :}
--   True
--
--   >>> :{
--     checkEval (area (Rectangle 3 5)) (App areaExp (app2 rectangleExp (num 3) (num 5)))
--   :}
--   True
--
--   >>> :{
--     checkEval (area (square 4)) (App areaExp (App squareExp (num 4)))
--   :}
--   True
--
--   >>> :{
--     checkEval (perimeter (Circle 3)) (App perimeterExp (App circleExp (num 3)))
--   :}
--   True
--
--   >>> :{
--     checkEval (perimeter (Rectangle 3 5)) (App perimeterExp (app2 rectangleExp (num 3) (num 5)))
--   :}
--   True
--
--   >>> :{
--     checkEval (perimeter (square 4)) (App perimeterExp (App squareExp (num 4)))
--   :}
--   True
-- 
checkEval :: Nat -> Exp -> Bool
checkEval n e = num n == eval e
