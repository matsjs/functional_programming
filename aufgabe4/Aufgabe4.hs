import Data.Char
import Numeric

newtype IN_0 = IN_0 Integer

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
    
