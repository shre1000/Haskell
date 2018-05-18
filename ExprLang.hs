

-- This module is for exercise set A, exercise 1 and exercise 2.
-- This also contains test case files.


module ExprLang
(ExprTree, Name, Env, eval, simplify, identifier, getId)
where

import Data.Char

type Name = String 
type Env  = [(Name,Int)]

data ExprTree = Add ExprTree ExprTree 
              | Mul ExprTree ExprTree 
              | Var Name 
              | Val Int 
			  | Sub ExprTree ExprTree
			  | Div ExprTree ExprTree
			  | Neg ExprTree
			  | Min ExprTree ExprTree
			  | Max ExprTree ExprTree
                deriving Show

-- Return a Just wrapping the string if it is a valid identifier;
-- Otherwise return a Nothing (if any non-identifier charaters occur)
identifier :: String -> Maybe String
identifier xs = 
  case (getId xs) of
    (xs@(_:_),[]) -> Just xs
    otherwise     -> Nothing

-- Extract an identifer from the beginning of a string and
-- return the identifier and the remaining string.
getId :: String -> (String,String)
getId []     = ([],[])
getId xs@(x:_)
    | isAlpha x || x == '_' = span (\y -> isAlphaNum y || y == '_') xs
    | otherwise             = ([],xs)
 
-- Evaluate an expression tree
eval :: ExprTree -> Env -> Int
eval (Add l r) env = eval l env + eval r env
eval (Mul l r) env = eval l env * eval r env
eval (Var n)   env =
  case (lookup n env) of
    Just i  -> i
    Nothing -> error ("Undefined variable " ++ show n)
eval (Val v)   _   = v
eval (Sub l r) env = eval l env - eval r env
eval (Div l r) env = eval l env  `div` eval r env
eval (Neg t) env = - eval t env
eval (Min l r) env = if(eval l env >= eval r env) then 
                       eval r env
					 else 
                        eval l env
					    
					      
eval (Max l r) env = if(eval l env >= eval r env) then 
                       eval l env
					 else 
                        eval r env
					  
-- Simplifiy an expression tree
simplify :: ExprTree -> ExprTree
simplify (Add l r) =
  case (simplify l, simplify r) of
    (Val 0, rr)    -> rr
    (ll, Val 0)    -> ll
    (Val x, Val y) -> Val (x+y)
    (ll, rr)       -> Add ll rr
simplify (Mul l r) =
  case (simplify l, simplify r) of
    (Val 0, rr)    -> Val 0
    (ll, Val 0)    -> Val 0
    (Val 1, rr)    -> rr
    (ll, Val 1)    -> ll
    (Val x, Val y) -> Val (x*y)
    (ll, rr)       -> Mul ll rr
simplify t@(Var _) = t
simplify t@(Val _) = t
simplify (Sub l r) =
  case (simplify l, simplify r) of
    (Val 0, rr)    -> Neg(rr)
    (ll, Val 0)    -> ll
    (Val x, Val y) -> Val (x-y)
    (ll, rr)       -> Sub ll rr
simplify (Div l r) =
  case (simplify l, simplify r) of
    (ll, Val 0)    -> error("infinity")
    (Val 0, rr)    -> Val 0
    (ll, Val 1)    -> ll
    (Val x, Val y) -> Val (x `div` y)
    (ll, rr)       -> Div ll rr
simplify (Neg t) =
  case (simplify t) of
    (Val 0)        -> Val 0
    (Val x)    ->  (Val (-x))
    (ll) -> Neg ll
simplify (Min l r) =
  case (simplify l, simplify r) of
    (Val x, Val y) -> if(x>=y) then 
	                   Val y 
					  else
					   Val x
    (ll , rr) -> Min ll rr
simplify (Max l r) =
  case (simplify l, simplify r) of
    (Val x, Val y) -> if(x>=y) then Val x else Val y
    (ll, rr) -> Max ll rr

	
main =
  do
    let env = [("x",5), ("y",7),("z",1)]
    let exp1 = Val 3                   
    let exp2 = Var "x"                 
    let exp3 = Add (Var "x") (Var "y")    
    let exp4 = Add (Var "x") (Val 0)   
    let exp5 = Mul (Sub (Val 2) (Val 3))
                   (Add (Val 1) (Val 0))
    let exp6 = Div (Div (Val 5) (Val 1)) (Neg (Val 3)) 
    let exp7 = Sub (Div (Val 5) (Val 2)) 
                   (Neg (Val (-5)))
    let exp8 = Sub (Val 0) (Val 5)                  
    let exp9 = Div (Val 0) (Val 5)               
    let exp10 = Neg (Neg (Val (-8)))
    let exp11 = Min (Val 8) (Val 9)
    let exp12 = Add (Mul (Val 5) (Val 0))
                    (Div (Var "x") (Var "z"))
    let exp13 = Max (Val 8) (Val 9)                
    let exp14 = Mul (Val 1) (Neg (Val 5))                
    let exp15 = Min (Add (Var "x") (Var "y"))
                    (Sub (Val 5) (Val 8))	
    let exp16 = Max (Add (Var "x") (Var "y"))
                    (Sub (Val 5) (Val 8))	
    let exp17 = Max (Var "x") (Var "z")
    let exp18 = Div (Val 1) (Val 0)
    putStrLn ("Expression: " ++ show exp1) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp1 env))
    putStrLn ("Simplification:  " ++ show (simplify exp1))
    putStrLn ("Expression: " ++ show exp2) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp2 env))
    putStrLn ("Simplification:  " ++ show (simplify exp2))
    putStrLn ("Expression: " ++ show exp3) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp3 env))
    putStrLn ("Simplification:  " ++ show (simplify exp3))
    putStrLn ("Expression: " ++ show exp4) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp4 env))
    putStrLn ("Simplification:  " ++ show (simplify exp4))
    putStrLn ("Expression: " ++ show exp5) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp5 env))
    putStrLn ("Simplification:  " ++ show (simplify exp5))
    putStrLn ("Expression: " ++ show exp6) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp6 env))
    putStrLn ("Simplification:  " ++ show (simplify exp6))
    putStrLn ("Expression: " ++ show exp7) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp7 env))
    putStrLn ("Simplification:  " ++ show (simplify exp7))
    putStrLn ("Expression: " ++ show exp8) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp8 env))
    putStrLn ("Simplification:  " ++ show (simplify exp8))
    putStrLn ("Expression: " ++ show exp9) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp9 env))
    putStrLn ("Simplification:  " ++ show (simplify exp9))
    putStrLn ("Expression: " ++ show exp10) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp10 env))
    putStrLn ("Simplification:  " ++ show (simplify exp10))
    putStrLn ("Expression: " ++ show exp11) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp11 env))
    putStrLn ("Simplification:  " ++ show (simplify exp11))
    putStrLn ("Expression: " ++ show exp12) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp12 env))
    putStrLn ("Simplification:  " ++ show (simplify exp12))
    putStrLn ("Expression: " ++ show exp13) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp13 env))
    putStrLn ("Simplification:  " ++ show (simplify exp13))
    putStrLn ("Expression: " ++ show exp14) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp14 env))
    putStrLn ("Simplification:  " ++ show (simplify exp14))
    putStrLn ("Expression: " ++ show exp15) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp15 env))
    putStrLn ("Simplification:  " ++ show (simplify exp15))
    putStrLn ("Expression: " ++ show exp16) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp16 env))
    putStrLn ("Simplification:  " ++ show (simplify exp16))
    putStrLn ("Expression: " ++ show exp17) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp17 env))
    putStrLn ("Simplification:  " ++ show (simplify exp17))
    putStrLn ("Expression: " ++ show exp18) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (eval exp18 env))
    putStrLn ("Simplification:  " ++ show (simplify exp18))
