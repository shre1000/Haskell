
-- This module is test case module for exercise set A, exercise 1 and exercise 2.

module ExprLangTest

where
import ExprLang

main = 
  do
    let env = [("x",5), ("y",7),("z",1)]
    let exp1 = Val 3          -- 3
    let exp2 = Var "x"   -- x
    let exp3 = Add (Val 0) (Val 5)  -- 0+5
   -- let exp4 = Add (Val 5) (Val 0)  -- 5+0
    let exp5 = Add (Val 5) (Val 5)  -- 5+5
   -- let exp6 = Add (Var "x") (Val 3)  -- x+3 
    let exp7 = Mul (Add (Var "x") (Var "y")) (Add (Val 2) (Var "z")) -- (x+y) * (2+z)
    let exp8 = Add (Sub (Val 2) (Val 5)) (Sub (Val 3) (Val 2))  -- (2-5) + (3-2)
    let exp9 = Mul (Val 1) (Val 2)     -- (1*2)
    let exp10 = Mul (Var "x") (Val 3)   -- x*3
    let exp11 = Mul (Div (Var "x") (Var "y"))(Add (Val 2) (Var "z")) -- (x/y) * (2+z)
    let exp12 = Mul (Min (Val 2) (Val 5)) (Max (Val 3) (Val 2)) -- Min 2 5 * Max 3 2 
-- let exp13 = Sub (Val 1) (Val 2)    -- 1-2
    let exp14 = Sub (Sub (Var "x") (Val 3)) (Sub (Val 1) (Val 2))-- x-3 - 1-2
    let exp15 = Sub (Neg(Val (-5))) (Neg(Val 5)) -- -(-5) - (-5)
    let exp16 = Sub (Div (Val 2) (Val 5)) (Mul (Val 3) (Val 2)) -- 2/5 - 3*2
	--let exp17 = Div (Val 1) (Val 2)    -- 1/2
    let exp18 = Div (Var "x") (Val 3)  -- x/3
    let exp19 = Div (Div (Var "x") (Var "y")) (Neg(Val 4)) -- (x/y) / (-4)
    let exp20 = Neg (Val 1)     -- -1
    let exp21 = Neg (Var "x")  -- -x
    let exp22 = Neg (Val (-5)) -- -(-5)
    --let exp23 = Min (Val 1) (Val 1)  -- min 1 1  
    --let exp24 = Min (Var "x") (Val 3)   -- min x 3
    --let exp25 = Min (Var "x") (Var "y") -- min x y
    let exp26 = Min (Min (Var "x") (Var "y")) (Min (Var "x") (Val 2)) -- min min x y Min x 2 
    --let exp27 = Max (Val 1) (Val 1)    -- max 1 1 
   -- let exp28 = Max (Var "x") (Val 3)  -- max x 3
    --let exp29 = Max (Var "x") (Var "y") -- max x y
    let exp30 = Max (Max (Var "x") (Var "y")) (Div (Val 3) (Val 2)) -- max max x y  3/2
    let exp31 = Sub (Val 0) (Val 5) -- 0-5
    --let exp32 = Sub (Val 5) (Val 0) -- 5-0
    --let exp34 = Mul (Val 0) (Val 5) -- 0*5
    let exp35 = Add (Mul (Val "x") (Val "y"))  (Mul (Neg (Val 0)) (Val 5)) -- x*y + -0*5
    let exp36 = Div (Val 0) (Val 5) -- 0/5
    let exp37 = Div (Val 2) (Val 1) -- 2/1
    --let exp38 = Neg (Val 0) -- -0
    let exp39 = Add (Neg(Val 5)) (Neg(Mul (Var "z") (Var "z"))) -- -5 + -(z*z)
    let exp40 = Div (Val 5) (Val 0)
	
    putStrLn ("Expression: " ++ show exp1) 
    putStrLn ("Evaluation: " ++ show (eval exp1 env))
    putStrLn ("Simplification: " ++ show (simplify exp1))
    putStrLn ""
	
	putStrLn ("Expression: " ++ show exp2) 
    putStrLn ("Evaluation: " ++ show (eval exp2 env))
    putStrLn ("Simplification: " ++ show (simplify exp2))
    putStrLn ""

	putStrLn ("Expression: " ++ show exp3) 
    putStrLn ("Evaluation: " ++ show (eval exp3 env))
    putStrLn ("Simplification: " ++ show (simplify exp3))
    putStrLn ""
	
	putStrLn ("Expression: " ++ show exp5) 
    putStrLn ("Evaluation: " ++ show (eval exp5 env))
    putStrLn ("Simplification: " ++ show (simplify exp5))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp7) 
    putStrLn ("Evaluation: " ++ show (eval exp7 env))
    putStrLn ("Simplification: " ++ show (simplify exp7))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp8) 
    putStrLn ("Evaluation: " ++ show (eval exp8 env))
    putStrLn ("Simplification: " ++ show (simplify exp8))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp9) 
    putStrLn ("Evaluation: " ++ show (eval exp9 env))
    putStrLn ("Simplification: " ++ show (simplify exp9))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp10) 
    putStrLn ("Evaluation: " ++ show (eval exp10 env))
    putStrLn ("Simplification: " ++ show (simplify exp10))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp11) 
    putStrLn ("Evaluation: " ++ show (eval exp11 env))
    putStrLn ("Simplification: " ++ show (simplify exp11))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp12) 
    putStrLn ("Evaluation: " ++ show (eval exp12 env))
    putStrLn ("Simplification: " ++ show (simplify exp12))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp14) 
    putStrLn ("Evaluation: " ++ show (eval exp14 env))
    putStrLn ("Simplification: " ++ show (simplify exp14))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp15) 
    putStrLn ("Evaluation: " ++ show (eval exp15 env))
    putStrLn ("Simplification: " ++ show (simplify exp15))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp16) 
    putStrLn ("Evaluation: " ++ show (eval exp16 env))
    putStrLn ("Simplification: " ++ show (simplify exp16))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp18) 
    putStrLn ("Evaluation: " ++ show (eval exp18 env))
    putStrLn ("Simplification: " ++ show (simplify exp18))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp19) 
    putStrLn ("Evaluation: " ++ show (eval exp19 env))
    putStrLn ("Simplification: " ++ show (simplify exp19))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp20) 
    putStrLn ("Evaluation: " ++ show (eval exp20 env))
    putStrLn ("Simplification: " ++ show (simplify exp20))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp21) 
    putStrLn ("Evaluation: " ++ show (eval exp21 env))
    putStrLn ("Simplification: " ++ show (simplify exp21))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp22) 
    putStrLn ("Evaluation: " ++ show (eval exp22 env))
    putStrLn ("Simplification: " ++ show (simplify exp22))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp26) 
    putStrLn ("Evaluation: " ++ show (eval exp26 env))
    putStrLn ("Simplification: " ++ show (simplify exp26))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp30) 
    putStrLn ("Evaluation: " ++ show (eval exp30 env))
    putStrLn ("Simplification: " ++ show (simplify exp30))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp31) 
    putStrLn ("Evaluation: " ++ show (eval exp31 env))
    putStrLn ("Simplification: " ++ show (simplify exp31))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp35) 
    putStrLn ("Evaluation: " ++ show (eval exp35 env))
    putStrLn ("Simplification: " ++ show (simplify exp35))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp36) 
    putStrLn ("Evaluation: " ++ show (eval exp36 env))
    putStrLn ("Simplification: " ++ show (simplify exp36))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp37) 
    putStrLn ("Evaluation: " ++ show (eval exp37 env))
    putStrLn ("Simplification: " ++ show (simplify exp37))
    putStrLn ""

    putStrLn ("Expression: " ++ show exp39) 
    putStrLn ("Evaluation: " ++ show (eval exp39 env))
    putStrLn ("Simplification: " ++ show (simplify exp39))
    putStrLn ""
