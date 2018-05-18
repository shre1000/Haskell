
-- This is test module.
module Test
where

import Demo
--Demo is source code module.

import Data.List
import Data.Function
import Data.Tuple

--showlist_my :: [a] -> String
--showlist_my b = 
main :: IO()
main =
        do 
						let b1 = ["kisses","snickers"]
						let b2 =["kisses"]
						let c1 = 1
						let c2 = 2
						let c3 = 10
						
						putStrLn("b1 = " ++ show(b1))
						putStrLn("b2 = " ++show(b2))
						putStrLn("c1 = " ++ show(c1))
						putStrLn("c2 = " ++ show(c2))
						putStrLn("c3 = " ++ show(c3))
						
						--putStr("newBowl is " ++ show(newBowl))
						
						putStrLn("put b1 c1 " ++ show(put b1 c1))
						putStrLn("put [] c1 " ++ show(put [] c1))
						
						putStrLn("takeCandy b1 c1 " ++ show(takeCandy b1 c1))
                        --putStrLn("takeCandy b1 c3 " ++ show(takeCandy b1 c3))
						--putStrLn("takeCandy [] c1 " ++ show(takeCandy [] c1))
						--putStrLn("takeCandy b2 c2 " ++ show(takeCandy b2 c2))
						
						putStrLn("isEmpty  b1 " ++ show(isEmpty b1))
						putStrLn("isEmpty [] " ++ show(isEmpty []))
						
						putStrLn("size b1 " ++ show(size b1))
						putStrLn("size [] " ++ show(size []))
						
						putStrLn("howMany b1 c1: " ++ show(howMany b1 c1))
                        --putStrLn("howMany b1 c3 " ++ show(howMany b1 c3))
						putStrLn("howMany [lol] lpl " ++ show(howMany ["lol"] "lpl"))
						putStrLn("howMany [] c1 " ++ show(howMany [] c1))
						
						putStrLn("has b1 c1 " ++ show(has b1 c1))
                        --putStrLn("has b1 c3 " ++ show(has b1 c3))
						putStrLn("has [] c1 " ++ show(has [] c1))
						
						putStrLn("eqBowl [1,2,3] [1,3,2] " ++ show(eqBowl [1,2,3] [1,3,2]))
						putStrLn("eqBowl [] [1,3,2] " ++ show(eqBowl [] [1,3,2]))
						putStrLn("eqBowl [1,2,3] [] " ++ show(eqBowl [1,2,3] []))
						putStrLn("eqBowl [1,2,3] [1,3] " ++ show(eqBowl [1,2,3] [1,3]))
						
					    --putStrLn("inventory b1 " ++ show(inventory b1))
						-- putStrLn("inventory [] " ++ show(inventory []))
						
						putStrLn("restock[(1,3), (2,1), (3,1)] " ++ show(restock[(1,3), (2,1), (3,1)]))
						--putStrLn("restock[] " ++ show(restock[]))
						
						putStrLn ("combine [1,2] [2] " ++ show(combine [1,2] [2]))
						putStrLn ("combine [1,2] [] " ++ show(combine [1,2] []))
						putStrLn ("combine [] [2] " ++ show(combine [] [2]))
						--putStrLn ("combine [] [] " ++ show(combine [] []))
						
						putStrLn ("subBowl [1,2,1] [1] " ++ show(subBowl [1,2,1] [1]))
						putStrLn ("subBowl [1,2,1] [] " ++ show(subBowl [1,2,1] []))
						--putStrLn ("subBowl [] [1] " ++ show(subBowl [] [1]))
						putStrLn ("subBowl [1,2,1] [55] " ++ show(subBowl [1,2,1] [55]))
						--putStrLn ("subBowl [] [] " ++ show(subBowl [] []))
					
	  




