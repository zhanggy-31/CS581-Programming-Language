-- | Homework 2 template. See the homework description page for more details,
--   hints, and things to think about for each part.
module HW2 where

import HW1

--
-- * Part 1: Reverse Polish Notation
-- 

-- | Takes an expression and returns a string encoding of that expression in
--   Reverse Polish Notation (RPN).
--
--   >>> toRPN (Lit 3)
--   "3"
--
--   >>> toRPN e1
--   "2 3 4 * +"
--
--   >>> toRPN e2
--   "7 6 + 5 *"
--
--   >>> toRPN e3
--   "3 2 * 5 4 * +"
--
--   >>> elem (toRPN e4) ["8 7 9 * + 6 +", "8 7 9 * 6 + +"]
--   True
--   
toRPN :: Expr -> String
toRPN (Lit x) = show x
toRPN (Add x y) = toRPN x ++ " " ++ toRPN y ++ " +"
toRPN (Mul x y) = toRPN x ++ " " ++ toRPN y ++ " *"


-- | Takes a string that is an RPN-encoded expression and produces the same
--   expression represented as an abstract syntax tree.
--
--   You can assume that your function will only be given valid RPN-encodings
--   of expressions. That is, it need not fail gracefully if it encounters an
--   error. However, if you would like to improve the error handling, you are
--   welcome to change the type of your function and the doctests.
--
--   >>> fromRPN "3"
--   Lit 3
--
--   >>> fromRPN "2 3 +"
--   Add (Lit 2) (Lit 3)
--
--   >>> fromRPN "2 3 4 + +"
--   Add (Lit 2) (Add (Lit 3) (Lit 4))
--
--   >>> all (\e -> e == fromRPN (toRPN e)) [e1,e2,e3,e4]
--   True
--
fromRPN :: String -> Expr
fromRPN s = solve [] (words s)
  where
    solve (x:[])   [] = x
    solve (x:y:ys) ("+":zs) = solve (Add y x:ys) zs
    solve (x:y:ys) ("*":zs) = solve (Mul y x:ys) zs
    solve ys (z:zs) = solve (Lit (read z): ys) zs
-- head, last, get the first and the last element of a list
-- tail, init, get the elements except the first and the last one, and remove the first and the last one.
-- words, separate every elements.
-- String is Syntactic Sugar of [Char]
-- the input of fromRPN is 1 string, and output of fromRPN should be 1 Expr.
-- the RPN can be decode reversely upon list. For example, "2 3 4 + +", when last char == "+"
-- or "*", add or multiple the first number with the rest strings and so on, when the last
-- char is a number, the function finishes. 
-- fromRPN 
--   | last(words (x:xs)) == "+" = Add(Lit (read (head (words (x:xs))))) (fromRPN (init xs))
--   | last(words (x:xs)) == "*" = Mul(Lit (read (head (words (x:xs))))) (fromRPN (init xs))
--   | otherwise Lit (read xs)
-- for "2 3 4 + +", it shows Add (Lit 2) (Add (Lit 3) (Add (Lit 3) (Lit 4)))
-- for "2 3 4 5 + + +", it shows Add (Lit 2) (Add (Lit 3) (Add (Lit 3) (Add (Lit 4) (Add (Lit 4) (Lit 5)))))
-- It seems only one "+" read once, and the others read twice.
-- also, this way only can deal with the situation that + and * at the end of RPN.
--
-- another way is pretending using stack, it deal with RPN one by one. push numbers into stack, when it meets
-- "+" or "*", pop top two numbers as the factors of add or mul. 
-- (x:y:ys) is the normal order of list, when meets "+", pretend a stack order of [ top y x bottom ], 
-- and pop y with Add y x:ys, ys are the unprocessed elements. in this way, actualy pop 1 number when meets + or *
-- like 2 + ys, 2 + 3 + ys.  
-- 


-- * Part 2: Syntactic Sugar
--
-- | Takes an expression and returns an expresion that evaluates to its
--   negation. Notice that this function does *not* evaluate the expression!
--   It returns a new expression that, when evaluated, will evaluate to the
--   negation of the original expression.
--
--   >>> eval e2
--   65
--
--   >>> eval (neg e2)
--   -65
--
neg :: Expr -> Expr
neg = \x -> Mul (Lit (-1)) x
-- the input of neg is 1 Expr, and output of neg should be 1 Expr. 
-- To negate a Expr, it can multiple the Expr with -1, for (7 + 6) * 5, 
-- (- (7 + 6) * 5) is the its negative without calculate the Expr at once.
-- " neg x = Mul (Lit (-1)) x " is equal to " neg = \x -> Mul (Lit (-1)) x "


-- | Takes two expressions and returns an expression that evalautes to the
--   second expression subtracted from the first. Once again, note that the
--   return type is an expression.
--
--   >>> eval e1
--   14
--
--   >>> eval (sub e2 e1)
--   51
--
sub :: Expr -> Expr -> Expr
sub = \x y -> Add x (neg y)
-- the inputs of sub are 2 Expr, and output of sub should be 1 Expr.
-- To express x - y in Add and Mul way, it can be x + (-1 * y)
-- sub x y = Add x (neg y) is equal to sub = \x y -> Add x (neg y)
