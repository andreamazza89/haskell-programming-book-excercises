module Hutton where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit int) = int
eval (Add expA expB) = (eval expA) + (eval expB)

printExpr :: Expr -> String
printExpr (Lit int) = show int
printExpr (Add expA expB) = (printExpr expA) ++ " + " ++ (printExpr expB)
