
-- This module is for exercise set C, exercise 1 to 5.
-- This also contains test cases.

module SInstr
(Name, Env, SInstr, SState, execSInstr, execSeq, exec, ExprTree, compile, compGo)
where

import Data.Char
import Data.List

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

data SInstr = SVal Int
            | SVar String
            | SPop
            | SSwap
            | SDup
            | SAdd
            | SMul
			| SSub
			| SDiv
			| SNeg
			| SMin
			| SMax
            deriving (Show, Eq)
                        

data SState = SState [Int] Int
              deriving (Show, Eq)

execSInstr :: SState -> Env -> SInstr -> (SState, Env)
-- push constant
execSInstr (SState es pc) env (SVal i) = (SState (i:es) (pc+1), env)
-- push variable
execSInstr (SState es pc) env (SVar v) = 
  case lookup v env of
    Just i -> (SState (i:es) (pc+1), env)
    Nothing -> error ("Variable " ++ show v ++ " undefined")
-- pop element
execSInstr  (SState es pc) env SPop  = 
  case es of 
    [] -> error ("Cannot pop. Stack is empty: " ++ show es)
    (x:xs) -> (SState xs (pc+1), env)
-- swap element
execSInstr  (SState es pc) env SSwap = 
  case es of 
    (x:y:xs) -> (SState (y:x:xs) (pc+1), env)
    _ -> error("can't swap. stack is too short: " ++ show es)
-- duplicate element
execSInstr  (SState es pc) env SDup  = 
  case es of 
    (x:xs) -> (SState (x:x:xs) (pc+1), env)
    [] -> error("can't duplicate. stack is too short: " ++ show es)
-- addition
execSInstr  (SState es pc) env SAdd  =
  case es of
    (r:l:xs) -> (SState ((l+r):xs) (pc+1), env)
    _        -> error ("Cannot Add. Stack too short: " ++ show es)
-- multiplication	
execSInstr  (SState es pc) env SMul = 
  case es of 
    (r:l:xs) -> (SState ((l*r):xs) (pc+1), env)
    _ -> error ("Cannot multiply. Stack too short: " ++ show es)
--subtraction
execSInstr  (SState es pc) env SSub = 
  case es of 
    (r:l:xs) -> (SState ((l-r):xs) (pc+1), env)
    _ -> error ("Cannot subtract. Stack too short: " ++ show es)
-- Division
execSInstr  (SState es pc) env SDiv = 
  case es of 
    (r:l:xs) -> (SState ((l `div` r):xs) (pc+1), env)
    _ -> error ("Cannot divide. Stack too short: " ++ show es)
-- Negation
execSInstr  (SState es pc) env SNeg = 
  case es of 
    (r:xs) -> (SState ((-r):xs) (pc+1), env)
    _ -> error ("Cannot do negation. Stack too short: " ++ show es)
-- minimum
execSInstr  (SState es pc) env SMin = 
  case es of 
    (r:l:xs) -> if (r > l) then 
	              (SState (l:xs) (pc+1), env)
				else
				   (SState (r:xs) (pc+1), env)
    _ -> error ("Cannot do minimum. Stack too short: " ++ show es)
-- maximum
execSInstr  (SState es pc) env SMax = 
  case es of 
    (r:l:xs) -> if (r > l) then 
	              (SState (r:xs) (pc+1), env)
				else
				   (SState (l:xs) (pc+1), env)
    _ -> error ("Cannot do maximum. Stack too short: " ++ show es)
	
execSeq :: SState -> Env -> [SInstr] -> (SState, Env)
execSeq (SState es pc) env (xs) = 
  case xs of
    (x:xs) -> execSeq (fst (SState es1 pc1, env1)) (snd (SState es1 pc1, env1)) (xs)
              where (SState es1 pc1, env1) = execSInstr (SState es pc) env (x)
    _ -> (SState es pc , env)

exec :: Env -> [SInstr] -> Int
exec env list = 
  case list of 
    (x:xs) -> case es2 of 
                (x:xs) -> x
                _ -> error ("nothing to show" ++ show es2)
              where (SState es2 pc2, env2) = execSeq  (SState [] 0) env list 
    _ -> error ("no instruction to execute" ++ show list)
	
compile :: ExprTree -> [SInstr]
compile (Add l r) = compile l ++ compile r ++ [SAdd]
compile (Sub l r) = compile l ++ compile r ++ [SSub]
compile (Mul l r) = compile l ++ compile r ++ [SMul]
compile (Div l r) = compile l ++ compile r ++ [SDiv]
compile (Min l r) = compile l ++ compile r ++ [SMin]
compile (Max l r) = compile l ++ compile r ++ [SMax]
compile (Neg t) = compile t ++ [SNeg]
compile (Val v) = [SVal v]
compile (Var n) = [SVar n]

compGo :: Env -> ExprTree -> Int
compGo env expr = exec env instruction
                  where instruction = compile expr


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
    putStrLn ("testing compGo function:")
    putStrLn ("Expression: " ++ show exp1) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp1))
    putStrLn ("Expression: " ++ show exp2) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp2))
    putStrLn ("Expression: " ++ show exp3) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp3))
    putStrLn ("Expression: " ++ show exp4) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp4))
    putStrLn ("Expression: " ++ show exp5) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp5))
    putStrLn ("Expression: " ++ show exp6) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp6))
    putStrLn ("Expression: " ++ show exp7) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp7))
    putStrLn ("Expression: " ++ show exp8) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp8))
    putStrLn ("Expression: " ++ show exp9) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp9))
    putStrLn ("Expression: " ++ show exp10) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp10))
    putStrLn ("Expression: " ++ show exp11) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp11))
    putStrLn ("Expression: " ++ show exp12) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp12))
    putStrLn ("Expression: " ++ show exp13) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp13))
    putStrLn ("Expression: " ++ show exp14) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp14))
    putStrLn ("Expression: " ++ show exp15) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp15))
    putStrLn ("Expression: " ++ show exp16) 
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp16))
    putStrLn ("Expression: " ++ show exp17)
    putStrLn ("Evaluation with x=5, y=7, z=1:  " ++ show (compGo env exp17))
    putStrLn ("testing pop operation: execSInstr (SState [5,2,3] 0) env SPop")
    putStrLn ("SPop is: " ++ show (execSInstr (SState [5,2,3] 0) env SPop))
    putStrLn ("testing swap operation: execSInstr (SState [5,2,3] 0) env SSwap")
    putStrLn ("SSwap is: " ++ show (execSInstr (SState [5,2,3] 0) env SSwap))
    putStrLn ("testing duplicate operation: execSInstr (SState [5,2,3] 0) env SDup")
    putStrLn ("SDup is: "  ++ show (execSInstr (SState [5,2,3] 0) env SDup))
	
    
