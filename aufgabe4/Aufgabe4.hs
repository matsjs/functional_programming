import Data.Char
import Data.List
import Numeric

newtype IN_0 = IN_0 Integer

-- Aufgabe 1

instance Show IN_0 where
    show (IN_0 num) = (if num >= 0 then (showIntAtBase 8 intToDigit num "") else "Nicht gueltig")


-- Aufgabe 2
data Antwort3 = Ja | Nein | Teilsteils deriving (Eq,Ord,Show)
type A3 = Antwort3

class Gueltig a where
    ist_gueltig :: a -> A3

instance Gueltig IN_0 where
    ist_gueltig(IN_0 num) = (if num >= 0 then Ja else Nein)

-- Aufgabe 3

instance Gueltig Int where
    ist_gueltig(num) = Ja

instance Gueltig Integer where
    ist_gueltig(num) = Ja

instance Gueltig Double where
    ist_gueltig(num) = Nein

instance Gueltig Float where
    ist_gueltig(num) = if (toRational(num) == (fromInteger(floor num))) then Ja else Nein


-- Aufgabe 4
instance Main.Eq IN_0 where
    (IN_0 x) == (IN_0 y) = x == y

instance Ord IN_0 where
    compare (IN_0 x) (IN_0 y)
        |x == y = EQ 
        |x <= y = LT
        |otherwise = GT
    (IN_0 x) >= (IN_0 y) = compare x y /= LT
    (IN_0 x) <= (IN_0 y) = compare x y == LT
    (IN_0 x) > (IN_0 y) = compare x y == GT
    (IN_0 x) < (IN_0 y) = compare x y == LT
    max(IN_0 x) (IN_0 y) = IN_0 (max x y)
    min(IN_0 x) (IN_0 y) = IN_0 (min x y)

instance Enum IN_0 where
    succ (IN_0 x)
        |(IN_0 x) > (IN_0 0) = IN_0(x+1)
        |otherwise = IN_0 0
    pred (IN_0 x) = IN_0 (x-1)
    toEnum x
        |x >= 0 = IN_0 (toInteger x)
        |otherwise = IN_0 0
    fromEnum (IN_0 x)
        |x>= 0 = fromIntegral x
        |otherwise = 0

instance Num IN_0 where
    (+) (IN_0 x) (IN_0 y) = IN_0 (x+y)
    (*) (IN_0 x) (IN_0 y) = IN_0 (x*y)
    fromInteger (x)
        |x >= 0 = IN_0 x
        |otherwise = IN_0 0
    negate (IN_0 x) = IN_0 (-x)
    signum (IN_0 x) = IN_0 (signum x)
    abs (IN_0 x) = IN_0(abs x)
    
-- Aufgabe 5
newtype Nat_Liste = NL [IN_0] deriving (Eq, Show)

test_validity :: Nat_Liste -> A3
test_validity (NL list)
    | list == [] = Ja
    | ist_gueltig (head list) == Ja = test_validity $ NL (tail list)
    | otherwise = Nein

test_invalidity (NL list)
    | list == [] = Nein
    | ist_gueltig (head list) == Nein = test_invalidity $ NL (tail list)
    | otherwise = Ja

instance Gueltig Nat_Liste where
    ist_gueltig (NL list)
        | test_validity(NL list) == Ja = Ja
        | test_invalidity(NL list) == Nein = Nein
        | otherwise = Teilsteils


-- Aufgabe 6

sum_calc :: Nat_Liste -> Integer
sum_calc (NL list)
    | list == [] = 0
    | otherwise = toInteger(fromEnum $ (head list)) + (sum_calc $ NL (tail list))

summe_integer :: Nat_Liste -> Integer
summe_integer (NL list)
    | ist_gueltig(NL list) == Ja = sum_calc(NL list)
    | otherwise = (-1)

summe_int :: Nat_Liste -> Int
summe_int (NL list) = fromIntegral $ (summe_integer(NL list))

list_maker :: [IN_0] -> Int -> IN_0 -> [Nat_Liste]
list_maker list len n = [NL (sort[(list !! x),(list !! y),(list !! z)]) | x <- [0..len], y <- [x..len], z <- [y..len], x /= y, x /= z, y /= z, (list !! x) + (list !! y) + (list !! z) == n]

tripel_finder :: Nat_Liste -> IN_0 -> [Nat_Liste]
tripel_finder (NL list) (IN_0 num)
    | ist_gueltig(NL list) == Nein || ist_gueltig(NL list) == Teilsteils = []
    | otherwise = nub(list_maker (sort(list)) (length(list) - 1) (IN_0 num))
    