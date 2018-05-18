module Test_fun_3_5_6
where

import Fun3_5_6

main =
    do	
					putStrLn ("mult 1 0 = " ++ show(mult 1 0))
					putStrLn ("mult 10 20 = " ++ show(mult 10 20))
					putStrLn ("mult 0 10 = " ++ show(mult 0 10))
					putStrLn ("mult 5 1 = " ++ show(mult 5 1))
					putStrLn ("mult 1 0 = " ++ show(mult 1 0))
					putStrLn ("div23n5 30 = " ++ show(div23n5 30))
					putStrLn ("div23n5 2 = " ++ show(div23n5 2))
					putStrLn ("div23n5 3 = " ++ show(div23n5 3))
					putStrLn ("div23n5 5 = " ++ show(div23n5 5))
					putStrLn ("div23n5 6 = " ++ show(div23n5 6))
					putStrLn ("div23n5 10 = " ++ show(div23n5 10))
					putStrLn ("div23n5 11 = " ++ show(div23n5 11))
					putStrLn ("div23n5 15 = " ++ show(div23n5 15))
					putStrLn ("div23n5 1 = " ++ show(div23n5 1))
					putStrLn ("addTax 1.0 9.0 = " ++ show(addTax 1.0 9.0))
					putStrLn ("addTax 15.0 9.0 = " ++ show(addTax 15.0 9.0))
					putStrLn ("addTax 20.15 9.0 = " ++ show(addTax 20.15 9.0))
					putStrLn ("subTax (addTax 1.0 9.0) 9.0 = " ++ show(subTax(addTax 1.0 9.0) 9.0))
					putStrLn ("subTax (addTax 15.0 9.0) 9.0 = " ++ show(subTax(addTax 15.0 9.0) 9.0))
					putStrLn ("subTax (addTax 20.15 9.0) 9.0 = " ++ show(subTax(addTax 20.15 9.0) 9.0))
