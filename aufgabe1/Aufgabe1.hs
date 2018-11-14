--
-- Task 1 --
--
h :: Int -> Int
h n
    |n>0 = powSum n n * h (n-1)
    |n<0 = n
    |otherwise = 1

-- Sums powers of x from x down to 0
powSum x n
    |n>0 = x^n + powSum x (n-1)
    |n==0 = 1
    |otherwise = 0


--
-- Task 2--
--

-- Primality check through: elem x primes
primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

-- Factorizer
factorize :: Integer -> [Integer]
factorize n = divs n primes
    where
    divs n ds@(d:t) | d*d > n    = [n | n > 1]
                    | r == 0     =  d : divs q ds
                    | otherwise  =      divs n t
            where  (q,r) = quotRem n d

-- Integer to list of digits
digits :: Integer -> [Integer]
digits = map (read . (:[])) . show


-- Sum integers of a list recursively, to solve for multidigit numbers
sumList :: [Integer] -> Integer
sumList n
    |n == [] = 0
    |head(n) < 10 = head(n) + sumList(tail(n))
    |head(n) >= 10 = (sum(digits(head(n)))) + sumList(tail(n))
    |otherwise = 0

-- Main function
type Nat = Integer
klassifiziere :: Nat -> String
klassifiziere n
    |not (length(factorize(n)) == 1) && (sum(digits(n)) == sumList(factorize(n))) = "echt quer"
    |(length(factorize(n)) == 1) && (sum(digits(n)) == sumList(factorize(n))) = "unecht quer"
    |otherwise = "nicht quer"


--
-- Task 3 --
--
kbb x y = (sqrt(x^2 + y^2) + x + y)*0.5