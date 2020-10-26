module TypeKwnoDo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk toB a b = (toB a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith toB int a = (toB a) + (fromInteger int)
