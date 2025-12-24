module Set2b where

-- import Mooc.Todo

-- Some imports you'll need. Don't add other imports :)
import Data.List
import Mooc.Todo (todo)
import Data.Bool (Bool(False))
import Text.Read.Lex (numberToFixed)

------------------------------------------------------------------------------
-- Ex 1: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

-- binomial :: Integer -> Integer -> Integer
-- binomial :: Integer -> Integer -> Integer
-- binomial _ 0 = 1
-- binomial 0 _ = 0
-- binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

-- binomial n k
--   | k > n = undefined
--   | k == 0 = 1
--   | k == 1 = n
--   | k > (n `div` 2) = binomial n (n - k)
--   | otherwise = n * binomial (n - 1) (k - 1) `div` k


-- tail recursive version

-- Java version for reference:
  -- public static int binomial(int n, int k) {
  --   checkNonNegative("n", n);
  --   checkNonNegative("k", k);
  --   checkArgument(k <= n, "k (%s) > n (%s)", k, n);
  --   if (k > (n >> 1)) {
  --     k = n - k;
  --   }
  --   if (k >= biggestBinomials.length || n > biggestBinomials[k]) {
  --     return Integer.MAX_VALUE;
  --   }
  --   switch (k) {
  --     case 0:
  --       return 1;
  --     case 1:
  --       return n;
  --     default:
  --       long result = 1;
  --       for (int i = 0; i < k; i++) {
  --         result *= n - i;
  --         result /= i + 1;
  --       }
  --       return (int) result;
  --   }
  -- }

binomial :: Integer -> Integer -> Integer
-- using the java algorithm
binomial n k
  | k < 0 || n < 0 = error "n and k must be non-negative"
  | k > n = 0
  | k > n `div` 2 = binomial n (n - k)
  | k == 0 = 1
  | k == 1 = n
  | otherwise = go n k 1 0
  where
    go n k result i
      | i >= k = result
      | otherwise = go n k (result * (n - i) `div` (i + 1)) (i + 1)

------------------------------------------------------------------------------
-- Ex 2: implement the odd factorial function. Odd factorial is like
-- factorial, but it only multiplies odd numbers.
--
-- Examples:
--   oddFactorial 7 ==> 7*5*3*1 ==> 105
--   oddFactorial 6 ==> 5*3*1 ==> 15

oddFactorial :: Integer -> Integer
oddFactorial 1 = 1
oddFactorial n
  | n `mod` 2 == 0 = oddFactorial (n - 1)
  | otherwise = n * oddFactorial (n - 1)

------------------------------------------------------------------------------
-- Ex 3: implement the Euclidean Algorithm for finding the greatest
-- common divisor:
--
-- Given two numbers, a and b,
-- * if one is zero, return the other number
-- * if not, subtract the smaller number from the larger one
-- * replace the larger number with this new number
-- * repeat
--
-- For example,
--   myGcd 9 12 ==> 3
-- In this case, the algorithm proceeds like this
--
--   a      b
--
--   9      12
--   9      (12-9)
--   9      3
--   (9-3)  3
--   6      3
--   (6-3)  3
--   3      3
--   (3-3)  3
--   0      3
--
-- Background reading:
-- * https://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
myGcd a b
 | a <= 0 = b
 | b <= 0 = a
 | a >= b = myGcd (a-b) b
 | b >= a = myGcd a (b-a)
------------------------------------------------------------------------------
-- Ex 4: Implement the function leftpad which adds space characters
-- to the start of the string until it is long enough.
--
-- Examples:
--   leftpad "foo" 5 ==> "  foo"
--   leftpad "13" 3 ==> " 13"
--   leftpad "xxxxx" 3 ==> "xxxxx"
--
-- Tips:
-- * you can combine strings with the ++ operator.
-- * you can compute the length of a string with the length function

leftpad :: String -> Int -> String
leftpad str len =  concat (replicate (len - length str) " ")  ++ str

------------------------------------------------------------------------------
-- Ex 5: let's make a countdown for a rocket! Given a number, you
-- should produce a string that says "Ready!", counts down from the
-- number, and then says "Liftoff!".
--
-- For example,
--   countdown 4 ==> "Ready! 4... 3... 2... 1... Liftoff!"
--
-- Hints:
-- * you can combine strings with the ++ operator
-- * you can use the show function to convert a number into a string
-- * you'll probably need a recursive helper function

countdown :: Integer -> String
countdown num = "Ready! " ++ countdown' num
 where countdown' num
        | num == 0 = "Liftoff!"
        | num > 0 = show num ++ "... " ++ countdown' (num - 1)
------------------------------------------------------------------------------
-- Ex 6: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number evenly.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!
--
-- Hint: remember the mod function!

smallestDivisor :: Integer -> Integer
smallestDivisor number = smallestDivisor' number (number -1) number
                            where smallestDivisor' number potential_divisor definitie_divisor
                                      | potential_divisor == 1 || potential_divisor == 0 = definitie_divisor
                                      | number `mod` potential_divisor == 0 = smallestDivisor' number (potential_divisor - 1) potential_divisor
                                      | otherwise = smallestDivisor' number (potential_divisor -1) definitie_divisor
------------------------------------------------------------------------------
-- Ex 7: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime num = num == smallestDivisor num

------------------------------------------------------------------------------
-- Ex 8: implement a function biggestPrimeAtMost that returns the
-- biggest prime number that is less than or equal to the given
-- number. Use the function isPrime you just defined.
--
-- You don't need to care about arguments less than 2. Any behaviour
-- for them is fine.
--
-- Examples:
--   biggestPrimeAtMost 3 ==> 3
--   biggestPrimeAtMost 10 ==> 7

biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost number
 | isPrime number = number
 | otherwise = biggestPrimeAtMost (number - 1)
