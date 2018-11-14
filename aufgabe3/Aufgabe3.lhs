Task 1
Naive solution where we recursively compare elements of a
sorted list, in order to find the corresponing gaps
\begin{code}
type Nat0 = Int
type Zett = Int
type Terrain = [Zett]
type Schluchtenbreite = Nat0
type Schluchtenliste = [Terrain]
schluchtenfinder :: Terrain -> Schluchtenbreite -> Schluchtenliste

schluchtenfinder l w = plateau w 1 (quicksort l)

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

gap :: Int -> Int -> Int
gap x y
    |x == y = 0
    |otherwise = - (y - (x - 1))

plateau :: Int ->  Int -> [Int] -> [[Int]]
plateau w n l
    |n > 1 && n >= length l = [l]
    |n == length l = []
    |gap (l !! n) (l !! (n-1)) == w = plateau w (n+1) l
    |n > 1 && (gap (l !! n) (l !! (n-1)) /= w) =  [take n l] ++ (plateau w 1 (drop n l))
    |otherwise = (plateau w 1 (drop n l))
\end{code}


Task 2

Setting up the type and instanciating
\begin{code}

newtype IN_0 = IN_0 Integer deriving Show

instance Num IN_0 where
    IN_0 a + IN_0 b = IN_0 (a+b)
    IN_0 a - IN_0 b = IN_0 (a-b)
    IN_0 a * IN_0 b = IN_0 (a*b)
    fromInteger = IN_0

instance Eq IN_0 where
    IN_0 x == IN_0 y = x == y

instance Ord IN_0 where
    compare (IN_0 x) (IN_0 y) = compare x y

instance Real IN_0 where
    toRational (IN_0 x) = toRational x

instance Enum IN_0 where
    fromEnum (IN_0 x) = fromEnum x
    toEnum = IN_0 . toEnum

instance Integral IN_0 where
    toInteger (IN_0 x) = x
    quotRem (IN_0 x) (IN_0 y) = (IN_0 q, IN_0 r)
        where (q, r) = quotRem x y
\end{code}



Operationen
\begin{code}

nplus :: IN_0 -> IN_0 -> IN_0
nplus x y
    |x > 0 && y > 0 = x+y
    |otherwise = IN_0 0

nminus :: IN_0 -> IN_0 -> IN_0
nminus x y
    |x-y > 0 = x-y
    |otherwise = IN_0 0

nmal :: IN_0 -> IN_0 -> IN_0
nmal x y
    |x > 0 && y > 0 = x*y
    |otherwise = IN_0 0

ndurch :: IN_0 -> IN_0 -> IN_0
ndurch x y
    |x > 0 && y > 0 && x >= y = div x y
    |otherwise = IN_0 0

\end{code}


Relationen
\begin{code}
ngleich :: IN_0 -> IN_0 -> Bool
ngleich x y
    |x >= 0 && y >= 0 = x == y
    |otherwise = False

nungleich :: IN_0 -> IN_0 -> Bool
nungleich x y
    |x >= 0 && y >= 0 = x /= y
    |otherwise = False

ngroesser :: IN_0 -> IN_0 -> Bool
ngroesser x y
    |x >= 0 && y >= 0 = x > y
    |otherwise = False

nkleiner :: IN_0 -> IN_0 -> Bool
nkleiner x y
    |x >= 0 && y >= 0 = x < y
    |otherwise = False

ngrgleich :: IN_0 -> IN_0 -> Bool
ngrgleich x y
    |x >= 0 && y >= 0 = x >= y
    |otherwise = False

nklgleich :: IN_0 -> IN_0 -> Bool
nklgleich x y
    |x >= 0 && y >= 0 = x <= y
    |otherwise = False

ist_ngueltig :: IN_0 -> Bool
ist_ngueltig x = x >= IN_0 0

\end{code}




Task 3
Same as in Task 2, with minor modifications
\begin{code}
type Xp_Zf = String -- fuer eXPandierte ZeichenFolge
type Kp_Zf = String -- fuer KomPrimierte ZeichenFolge
opt_komp :: Xp_Zf -> Kp_Zf
opt_komp ez = countOccs ez 0

countOccs :: String -> Int -> String
countOccs s n 
    |n == length s  = (show n) ++ [head s]
    |head s == s !! n = countOccs s (n+1)
    |head s /= s !! n && n < 3 = charMult n [head s] ++ countOccs (drop n s) 0
    |head s /= s !! n = (show n) ++ [head s] ++ countOccs (drop n s) 0

opt_expnd :: Kp_Zf -> Xp_Zf
opt_expnd s
    |null s = ""
    |isInt (head s) && isInt (head (tail s)) =  (take((toInt(take 2 s))) . repeat $ (s !! 2)) ++ opt_expnd(drop 3 s)
    |isInt (head s) = (take((toInt([head s]))) . repeat $ (s !! 1)) ++ opt_expnd(drop 2 s)
    |otherwise = take 1 s ++ opt_expnd(drop 1 s)

charMult :: Int -> String -> String
charMult n c = take n (cycle c)

isInt :: Char -> Bool
isInt c = elem c ['0'..'9']

toInt :: String -> Int
toInt s = read s :: Int
\end{code}


Task 4
Expanded upon list comprehension from last excersize
\begin{code}
aufteilen3 :: String -> [(String,String,String)]
aufteilen3 s = [(take i s, take j (drop i s), drop (j+i) s) | i <- [0..length s] , j <- [0..length s -i]]
\end{code}
