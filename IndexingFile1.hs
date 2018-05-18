-- CSCI 556: Multiparadigm Programming 
-- Assignment #3, Indexing
-- Shreyasi Kokamthankar
-- Student ID: 10599411

-- This is source code module for 12.30, 12.31- lambda expressions, 12.33, 12.37, 12.38, 12.40.

module File1
(Doc, Line, Word1, lines1, modifyCapEqui, modifyCapEqui_Intermediate, 
numLines, numWords, whitespace, splitWords, split, dropSpace, getWord, dropWord, allNumWords, shorten, 
sortLs, orderPair, orderPair1, makeLists, amalgamate, printIndex, showIndex, makeIndex)
where

import  Data.Char

type Doc = String
type Line = String
type Word1 = String 

-- original code uses default lines function. I modified it according to 12.30.
-- This function separates lines.
lines1 :: Doc -> [Line]
lines1 d 
         |d /= [] =  [takeWhile (/= '\n') d] ++ lines1 (drop 1 (dropWhile (/= '\n') d))
		 |otherwise = [] 
		 
-- original program is modified according to 12.40. 
-- Question: This doesn't work well for proper names like "Amelia". What could you do about that?
-- Answer: One approach is to check whether previous word was "the" article or not. So if it was "the" then that can be name of place or object.
modifyCapEqui :: [Line] -> [Line]
modifyCapEqui xs = map modifyCapEqui_Intermediate xs

modifyCapEqui_Intermediate :: String -> String
modifyCapEqui_Intermediate = map toLower 

-- This is function from original program. 
-- This maps line to line number.
numLines :: [Line] -> [(Int , Line)]
numLines l = zip [1 .. length l] l

-- This is function from original program. 
-- This breaks single line into words and attach line number to them.
numWords :: (Int , Line) -> [(Int , Word1)]
numWords (number , line) = [ (number , word) | word <- splitWords line ]

-- This is function from original program.
-- This is whitespace declairation.
whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"

-- Following  set of functions are use by numWords function.

-- These are functions from original program.
-- split words
splitWords :: String -> [Word1]
splitWords st = split (dropSpace st)

split :: String -> [Word1]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

-- This is function from original program.
-- Drop the whitespace character from the string. 
dropSpace :: String -> String
dropSpace []              = []
dropSpace (x:xs) 
    | elem x whitespace   = dropSpace xs
    | otherwise           = (x:xs)

-- This is function from original program.
-- Get the front word of string
getWord :: String -> String
getWord []                = [] 
getWord (x:xs) 
    | elem x whitespace   = []
    | otherwise           = x : getWord xs

-- This is function from original program.
-- Drop the front words of string.	
dropWord :: String -> String
dropWord []               = []
dropWord (x:xs) 
    | elem x whitespace   = (x:xs)
    | otherwise           = dropWord xs

-- End of functions use by numWords.

-- This function is modified according to 12.38.
-- This breaks all line into words and attach line numbers to them and also remove words < 4.
allNumWords :: [( Int , Line)] -> [(Int , Word1)]
allNumWords  = shorten. concat . map numWords 

shorten :: [(Int,Word1)] -> [(Int,Word1)]
shorten = filter sizer 
    where
        sizer (nl,wd) = length wd > 3 

		
-- This function is modified according to 12.33.
-- This function sorts the list.
sortLs :: [(Int , Word1)] -> [(Int , Word1)]
sortLs []     = []
sortLs (p:ps) = sortLs smaller ++ (p:equal) ++ sortLs larger
    where 
      smaller = [ q | q<-ps , orderPair q p ]
      larger  = [ q | q<-ps , orderPair p q ]
      equal   = [ q | q<-ps , orderPair1 p q ]
	  
orderPair :: (Int , Word1) -> (Int , Word1) -> Bool
orderPair ( n1 , w1 ) ( n2 , w2 ) = w1 < w2 || ( w1 == w2 && n1 < n2 )

orderPair1 :: (Int , Word1) -> (Int , Word1) -> Bool
orderPair1 ( n1 , w1 ) ( n2 , w2 ) =  w1 == w2 && n1 == n2 

-- This function is modified according to 12.31 - lambda expressions.
-- make list
makeLists ::  [ (Int,Word1) ] -> [ ([Int],Word1) ]
makeLists = map (\(n,st) -> ([n],st))

-- This is original function.
-- combine the list of int of same words into one list.
amalgamate :: [ ([Int],Word1) ] -> [ ([Int],Word1) ]
amalgamate []   = []
amalgamate [p]  = [p]
amalgamate ((l1,w1):(l2,w2):rest)
    | w1 /= w2  = (l1,w1) : amalgamate ((l2,w2):rest)
    | otherwise = amalgamate ((l1++l2,w1):rest)
	
-- This function is developed according to 12.37.
printIndex :: [([Int], Word1)] -> IO()
printIndex xs = mapM_ (putStrLn . showIndex) xs

showIndex :: (Show c, Show d) => (c,d) -> String
showIndex (c,d) = (show d) ++ "          " ++ (show c)

-- This is the original function but has to modify it according to changes.
-- make index
makeIndex :: Doc -> IO()
makeIndex
  = lines1        >.>     --   Doc            -> [Line]
    modifyCapEqui >.>     --   [Line]         -> [Line]
    numLines      >.>     --   [Line]         -> [(Int,Line)] 
    allNumWords   >.>     --   [(Int,Line)]   -> [(Int,Word)]
    sortLs        >.>     --   [(Int,Word)]   -> [(Int,Word)]
    makeLists     >.>     --   [(Int,Word)]   -> [([Int],Word)]
    amalgamate    >.>     --   [([Int],Word)] -> [([Int],Word)]
	printIndex            --   [([Int],Word)] -> IO()
	
-- This is for >.> mention in makeIndex.
infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g
    
