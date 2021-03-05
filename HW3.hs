module HW3 where

import Prelude hiding (Enum(..), sum)
import Data.List


--
-- * Part 1: Run-length lists
--

-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--
compress :: Eq a => [a] -> [(Int,a)]
compress = map (\x -> (length x,head x)).group
-- group :: eq a =>[a] ->[[a]]
--
-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--  
decompress :: [(Int,a)] -> [a]
decompress = concatMap (uncurry replicate)
-- concatMap :: (a -> [b]) -> [a] -> [b], it creates a list that the second argument will be processed by the first argument.
-- curry and uncurry
-- curry: 	((a,b) -> c) -> a -> b -> c, it converts uncurried function to a curried function, like fun x y = x + y 
-- uncurry: (a -> b -> c) -> (a,b) -> c, it converts curried function to a paired funtions, like fun (x, y) = x + y
-- replicate: Int -> a -> [a], it creates a list by a tuple (x, y) which means the list contains x items which values y.
-- replicate (4,1) = [1,1,1,1]
--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat
   = Zero
   | Succ Nat
  deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--
-- A successor  of x is x + 1, a predecessor of x is x - 1. 
-- which means succ(x) = x + 1, and pred(x) = x - 1.
-- pred (succ x) = succ (pred x) = x
pred :: Nat -> Nat   
pred Zero = Zero
pred (Succ x) = x


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False


-- | Convert a natural number to an integer. NOTE: We use this function in
--   tests, but you should not use it in your other definitions!
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero = 0
toInt x = toInt(pred x) + 1
-- the base number is 0, successor should be its predecessor + 1. 


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--
add :: Nat -> Nat -> Nat   
add Zero x = x
add x y = add (pred x)(Succ y)
-- x + y = (x - 1) + (y + 1)


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub x Zero = x
sub x y = sub (pred x)(pred y)
-- x - y = (x - 1) - (y - 1) 


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt Zero y = False
gt x Zero = True
gt x y = gt (pred x) (pred y)
-- see first argument equals Zero, return False.


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult x Zero = Zero
mult x y = add x (mult x (pred y))
-- x * y = x + x * (y-1)


-- | Compute the sum of a list of natural numbers.
--
--   >>> sumall []
--   Zero
--   
--   >>> sumall [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sumall [one,two,three])
--   6
--
sumall :: [Nat] -> Nat
sumall [] = Zero
sumall (x:xs) = add x (sumall xs)
-- sum is ambiguous with Data.List.sum , so I change the name from 'sum' to 'sumall'


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sumall (take 100 odds))
--   10000
--
odds :: [Nat]
odds = (Succ Zero) : map(Succ . Succ) odds
-- [1,3,5,7,...]