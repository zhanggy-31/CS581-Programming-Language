module HW4Part1 where

import Prelude hiding (Num)

--
-- * Part 1
--

-- 1. Encode the abstract syntax for the language as a set of Haskell types and data types.
-- construct the data types and types based on the HW4 description

data Reg
   = A
   | B
   | R
   deriving(Eq, Show)

data Expr
   = Int Int
   | Bool Bool
   | Reg Reg
   | Add Expr Expr
   | LaE Expr Expr
   | Not Expr
   deriving(Eq, Show)

data Stmt
   = Store Reg Expr
   | if Expr
     then Prog
     else Prog
   deriving(Eq, Show)

type Prog
   = [Stmt]
 
----------------------------------
-- 2. Encode the AST of the example program above as a Haskell value.

example = [Store A (Int 7),
           Store B (Int 9), 
           Store R (Int 0), 
           if (LaE (Reg A) (Int 0))
           then []
           else [Store R (Add (Reg R) (Reg B)), Store A (Add (Reg A) (Int -1))]]

----------------------------------
-- 3. Define a function while :: Expr -> Prog -> Stmt that defines a standard while loop as syntactic sugar.

while :: Expr -> Prog -> Stmt
while x [y] = do
				 	[if x
	             	 then [y]
	             	 else break]

-- For example, 
-- Store A (Int 7)
-- Store B (Int 9)
-- Prog = [Store R (Add (Reg A) (Reg B))]
-- while (LaE (Reg A) (Reg B)) = if (LaE (Reg A) (Reg B))
--							   then [Store R (Add (Reg A) (Reg B))]
--							   else break
----------------------------------
-- 4.Write a Haskell function sumFromTo :: Int -> Int -> Prog that takes two integers x and y, 
-- and returns a program in the object language that sums all of the integers starting from x up to and including y, 
-- storing the result in R. You can assume that sumFromTo is only called with arguments where x ≤ y. For example, 
-- sumFromTo 5 8 should generate an object language program that, if evaluated, would yield 26 (5+6+7+8) in register R.

sumFromTo :: Int -> Int -> Prog
sumFromTo x y = [Store A (Int x), 
				 Store B (Int y),
				 Store R (Int 0), 
				 do 
				 	if (LaE (Reg A) (Reg B))
				 	then [Add (Reg R) (Reg A), Add (Reg A) (Int 1)]
				 	else break
				 ]

-- For example, 
-- sumFromTo 7 9 = [Store A (Int 7), 
--				 Store B (Int 9),
--				 Store R (Int 0), 
--				 do 
--				 	if (LaE (Reg A) (Reg B))
--				 	then [Add (Reg R) (Reg A), Add (Reg A) (Int 1)]
--				 	else break
--				 ]
--	loop 1st: 7 <= 9, R = 0 + 7 = 7, A = 7 + 1 = 8;
--	loop 2nd: 8 <= 9, R = 7 + 8 = 15, A = 8 + 1 = 9;
--  loop 3rd: 9 <= 9, R = 15 + 9 = 24, A = 9 + 1 = 10;
--	loop 4th: break;