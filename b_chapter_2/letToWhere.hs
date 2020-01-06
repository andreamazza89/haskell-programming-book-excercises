module LetToWhere where

-- let x = 3; y = 1000 in x * 3 + y


one :: Int
one = x * 3 + y
  where x = 3
        y = 1000

-- let y = 10; x = 10 * 5 + y in x * 5

two :: Int
two = x * 5
  where y = 10
        x = 10 * 5 + y



-- let x = 7
--     y = negate x
--     z = y * 10
-- in z / x + y

three :: Float
three = z / x + y
  where x = 7
        y = (-x)
        z = y * 10

