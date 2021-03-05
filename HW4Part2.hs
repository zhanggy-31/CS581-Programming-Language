module HW4Part2 where

import Prelude hiding (Num)

--
-- * Part 2
--

--A := not 3;             { not can only be applied to booleans }
--B := 4 + (5 <= 6);      { can't add an integer and a boolean }
--R := 8 <= 7;            { registers may only contain integers }
--if 2+3 then else end;   { condition must be a boolean }

-- For A , integer don't have 'not' operation 
-- For B , Add cannot be used between Int and Bool
-- For R , registers may only contain integers
-- conditions can only be a boolean

-- 5. Refactor the syntax of the language to eliminate the possibility of type errors. 
-- The new syntax should be able to express all of the type correct programs that could be 
-- represented before and none of the type incorrect ones. Write the grammar of the new syntax in a comment in your file.

int ::= (any integer) --integers

bool ::= True | False
      
reg ::= A  |  B  |  R --register names
      
expr_int ::= int --integer literal
           | reg --load from register
           | expr + expr --integer addition

expr_bool ::= bool --integer literal
           | not expr --boolean negation
           | expr_int <= expr_int  --less than or equal to
      
stmt  ::= reg := expr_int --store to register
               | if expr_bool --conditional statement
                 then prog 
                 else prog 
                 end 
               | do prog --loop until break
                 end 
               | break --break out of a loop
      
prog  ::= ε  |  stmt ; --prog sequence of statements

-- For expr_int, only int can be store in the register, and only int can be Added.
-- For expr_bool, results of not and <= are boolean
-- For stmt, register are int values, and conditions are boolean values.

-- 6. Encode the new abstract syntax as a set of Haskell types and data types.

data Reg
   = A
   | B
   | R
   deriving(Eq, Show)

data Expr_int
   = Int Int
   | Reg Reg
   | Add Expr Expr 
   deriving(Eq, Show)

data Expr_bool
   = Bool Bool
   | Reg Reg
   | LaE Expr Expr
   | Not Expr

data Stmt
   = Store Reg Expr_int
   | if Expr_bool
     then Prog
     else Prog
   deriving(Eq, Show)

type Prog
   = [Stmt]