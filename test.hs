-- newtype Money =
--     Money Integer deriving Show
  
-- instance Num Money where
--     Money a + Money b = Money (a+b)
--     Money a - Money b = Money (a-b)
--     Money a * Money b = Money (a*b)
--     abs (Money a) = Money (abs a)
--     signum (Money a) = Money (signum a)
--     fromInteger = Money

-- instance Eq Money where
--     Money x == Money y = x == y

-- instance Ord Money where
--     compare (Money x) (Money y) = compare x y

-- instance Real Money where
--     toRational (Money x) = toRational x

-- instance Enum Money where
--     fromEnum (Money x) = fromEnum x
--     toEnum = Money . toEnum

-- instance Integral Money where
--     toInteger (Money x) = x
--     quotRem (Money x) (Money y) = (Money q, Money r)
--         where (q, r) = quotRem x y

newtype IN_0 = IN_0 Integer deriving Show

-- nplus :: IN_0 -> IN_0 -> IN_0
-- nplus x y  
--     | IN_0 x