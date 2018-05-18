
-- This is test module.
module Test
where

import Demo
--Demo is source code module.

import Data.List
import Data.Function
import Data.Tuple

main :: IO()
main =
    do 
		        
				--putStrLn("new Bowl is " ++ show(newBowl))
				--This is not working in Test.hs but if you test it individually on ghci, it works.
								
				putStrLn("put [kiss, mango] kiss :  " ++ show(put ["kiss", "mango"] "kiss"))
				putStrLn("put [] kiss :  " ++ show(put [] "kiss"))
										
				putStrLn("takeCandy [kiss, mango]  kiss : " ++ show(takeCandy ["kiss", "mango"] "kiss"))
				putStrLn("takeCandy [kiss] kiss :  " ++ show(takeCandy ["kiss"] "kiss"))
										
				putStrLn("isEmpty  [kiss] :  " ++ show(isEmpty ["kiss"]))
				putStrLn("isEmpty [] : " ++ show(isEmpty []))
										
				putStrLn("size [kiss]  : " ++ show(size ["kiss"]))
				putStrLn("size [] :  " ++ show(size []))
									
				putStrLn("howMany [kiss] kiss :  " ++ show(howMany ["kiss"] "kiss"))
				putStrLn("howMany [kiss] mango  :  " ++ show(howMany ["kiss"] "mango"))
				putStrLn("howMany [] kiss :  " ++ show(howMany [] "kiss"))
										
				putStrLn("has [kiss] kiss  :  " ++ show(has ["kiss"] "kiss"))
				putStrLn("has [] lemon  :  " ++ show(has [] "lemon"))
										
				putStrLn("eqBowl [kiss, lemon] [lemon, kiss]  : " ++ show(eqBowl ["kiss", "lemon"] ["lemon", "kiss"]))
				putStrLn("eqBowl [] [kiss] : " ++ show(eqBowl [] ["kiss"]))
				putStrLn("eqBowl [kiss] []  : " ++ show(eqBowl ["kiss"] []))
				putStrLn("eqBowl [kiss] [lemon] : " ++ show(eqBowl ["kiss"] ["lemon"]))
										
				putStrLn("inventory [kiss, snickers] :  " ++ show(inventory ["kiss", "snickers"]))
										
				putStrLn("restock  [(kiss,2),(lemon,1),(snickers,1)]  : " ++ show(restock[("kiss",2), ("lemon",1), ("snickers",1)]))
										
				putStrLn("combine [kiss] [kiss] : " ++ show(combine ["kiss"] ["kiss"]))
				putStrLn("combine [kiss] [] :  " ++ show(combine ["kiss"] []))
				putStrLn("combine [] [kiss]  : " ++ show(combine [] ["kiss"]))
										
				putStrLn("subBowl [kiss, lemon] [kiss] :  " ++ show(subBowl ["kiss","lemon"] ["kiss"]))
				putStrLn("subBowl [kiss] [] :  " ++ show(subBowl ["kiss"] []))
				putStrLn("subBowl [] [kiss]  : " ++ show(subBowl [] ["kiss"]))
				putStrLn("subBowl [kiss,lemon] [snickers] :  " ++ show(subBowl ["kiss", "lemon"] ["snickers"]))
				
				putStrLn("new Bowl is " ++ showList(showshow(newBowl)))

									
					  
