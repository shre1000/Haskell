--This is source code module.
module Demo
(newBowl, put, takeCandy, isEmpty, size, howMany, has, eqBowl, inventory, restock, restock_intermediate, combine, subBowl)
where

import Data.List
import Data.Function
import Data.Tuple
 
newBowl :: [a]
newBowl = []
               
put :: [a] -> a -> [a]
put b c = c:b

takeCandy :: Eq a => [a] -> a -> [a]
takeCandy [] c = error "you are trying to eat candy from empty bowl."
takeCandy (x:xs) c = if c `elem` (x:xs) then
                        if x == c then
                           xs  
				        else 
					       takeCandy xs c
				     else 
					    error "candy is not in bowl"
						
isEmpty :: [a] -> Bool
isEmpty b = null b

size :: [a] -> Int
size b = length b

howMany :: Eq a => [a] -> a -> Int
howMany [] c = 0
howMany b c = length (filter (==c) b)

has :: Eq a => [a] -> a -> Bool
has [] c = False
has b c = if c `elem` b then 
            True
		  else 
		    False

eqBowl :: Eq a => Ord a => [a] -> [a] -> Bool
eqBowl b1 b2 = sort b1 == sort b2
				
inventory :: Ord a => [a] -> [(a, Int)]	
inventory [] = []
inventory b = map (\l@(x:xs) -> (x,length l)) . group . sort $ b  

restock :: [(a, Int)] -> [a]
restock [] = []
restock inv = concat (map restock_intermediate inv) 

restock_intermediate :: (a, Int) -> [a]
restock_intermediate t = if (snd t > 0) then
            [fst t] ++ restock_intermediate (fst t, ((snd t)-1))
		  else
		    []

combine :: [a] -> [a] -> [a]
combine [] [] = []
combine b1 [] = b1
combine [] b2 = b2
combine b1 b2 = b1 ++ b2

subBowl :: Eq a => [a] -> [a] -> [a]
subBowl [] [] = []
subBowl b1 [] = b1
subBowl [] b2 = []
subBowl b1 b2 = b1 \\ b2
