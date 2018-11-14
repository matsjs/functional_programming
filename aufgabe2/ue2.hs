--
-- Task 1
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
    |not (length(factorize(n)) == 1) && (sum(digits(n)) == sumList(factorize(n))) = "Echt quer"
    |(length(factorize(n)) == 1) && (sum(digits(n)) == sumList(factorize(n))) = "Unecht quer"
    |otherwise = "Nicht quer"

echt_quer_max :: Nat1 -> Nat1
echt_quer_max n
    |klassifiziere n == "Echt quer" = n 
    |n < 1 = 0
    |otherwise = echt_quer_max (n-1)

qa m n 
    |m <= n && m <= x = qa m (x-1) ++ x : []
    |otherwise = []
    where
        x = echt_quer_max n

quer_alle :: Nat1 -> Nat1 -> [Nat1]
quer_alle m n = reverse (qa m n)

--
-- Task 2
--
type Expandierte_Zeichenfolge = String
type Komprimierte_Zeichenfolge = String
komp :: Expandierte_Zeichenfolge -> Komprimierte_Zeichenfolge
komp ez = countOccs ez 0


countOccs :: String -> Int -> String
countOccs s n 
    |n == length s  = (show n) ++ [head s]
    |head s == s !! n = countOccs s (n+1)
    |head s /= s !! n = (show n) ++ [head s] ++ countOccs (drop n s) 0

-- "exp" is a protected name, therefore using "expand"
expnd :: Komprimierte_Zeichenfolge -> Expandierte_Zeichenfolge
expnd s
    |length s < 2 = ""
    |isInt (head s) && isInt (head (tail s)) =  (take((toInt(take 2 s))) . repeat $ (s !! 2)) ++ expnd(drop 3 s)
    |isInt (head s) = (take((toInt([head s]))) . repeat $ (s !! 1)) ++ expnd(drop 2 s)

charMult :: Int -> String -> String
charMult n c = take n (cycle c)

isInt :: Char -> Bool
isInt c = elem c ['0'..'9']

toInt :: String -> Int
toInt s = read s :: Int


--
-- Task 3
--

aufteilen2 :: String -> [(String,String)]
aufteilen2 s = [(take x s, drop x s) | x <- [0..length s]]

--
-- Task 3
--

type Nat1 = Integer
type Mine = [Nat1]
type Goldnugget = Nat1
type Katzengoldnugget = Nat1
type Ausbeute = ([Goldnugget],[Katzengoldnugget])
schuerfe :: Mine -> Ausbeute
schuerfe l = (reverse . quickSort $ [x | x <- l, length(squaredSum(x)) > 0, naivePerfSquare x],
    reverse . quickSort $ [y | y <-l, length(squaredSum(y)) > 0, not (naivePerfSquare y) ])


squaredSum i = [(x,y) |x <- [1..(t)], y <- [1..(t)], (x^2+y^2) ==  i]
    where
        t = floor . sqrt . fromIntegral $ i

naivePerfSquare :: Integer -> Bool
naivePerfSquare i = elem i [x^2 | x <- [0.. floor . sqrt . fromIntegral $ i]]

quickSort :: (Ord a) => [a] -> [a]  
quickSort [] = []  
quickSort (x:xs) =   
    let smallerSorted = quickSort [a | a <- xs, a <= x]  
        biggerSorted = quickSort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  