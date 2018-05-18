

-- This is source code module for 12.31 -lambda expression and list comprehension. and 12.41.
module File2
(Doc, Line, Word1, numLines,numWords, whitespace, splitWords, dropSpace,split, getWord, dropWord, allNumWords, 
sortLs, orderPair, makeLists, amalgamate, shorten, makeIndex)
where

type Doc = String
type Line = String
type Word1 = String 

-- take input document and split it line wise by \n 
--lines :: Doc -> [Line]
-- we are using default lines function in prelude.

-- This is original code.
-- it pairs each line with its line no.
numLines :: [Line] -> [(Int , Line)]
numLines l = zip [1 .. length l] l

-- This is original code.
numWords :: (Int , Line) -> [(Int , Word1)]
numWords (number , line) = [ (number , word) | word <- splitWords line ]

-- This is original code.
-- whitespace declairation.
whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"

-- following  set of functions are use by numWords.

-- This is original code.
-- split the string
splitWords :: String -> [Word1]
splitWords st = split (dropSpace st)

split :: String -> [Word1]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

-- This is original code.
--drop the whitespace character from the string 
dropSpace :: String -> String
dropSpace []              = []
dropSpace (x:xs) 
    | elem x whitespace   = dropSpace xs
    | otherwise           = (x:xs)


-- This is original code.
-- get the front word of string
getWord :: String -> String
getWord []                = [] 
getWord (x:xs) 
    | elem x whitespace   = []
    | otherwise           = x : getWord xs


-- This is original code.
-- drop the front words of string	
dropWord :: String -> String
dropWord []               = []
dropWord (x:xs) 
    | elem x whitespace   = (x:xs)
    | otherwise           = dropWord xs

-- end of functions use by numWords


-- This is original code.
--part b: do it for all lines.
allNumWords :: [( Int , Line)] -> [(Int , Word1)]
allNumWords  = concat . map numWords 


-- This function is modified according to 12.41.
-- Question: What is the type after new defination?
-- Answer: sortLs now is a higher order function.
-- sort list
sortLs :: (Ord a)=> (a->a->Bool)->[a] -> [a]
sortLs _ []     = []
sortLs f (p:ps) = (sortLs f smaller) ++ [p] ++ (sortLs f larger)
    where 
      smaller = [ q | q<-ps , f q p ]
      larger  = [ q | q<-ps , f p q ]
	  
orderPair :: (Int, Word1) -> (Int, Word1) -> Bool
orderPair ( n1 , w1 ) ( n2 , w2 ) = w1 < w2 || ( w1 == w2 && n1 < n2 )

-- This function is modified according to 12.31 - list comprehension.
-- make list
makeLists ::  [(Int,Word1)] -> [([Int],Word1)]
makeLists [] = []
makeLists ((no,wo):xs) =  [f (n,w) | (n,w) <- [(no,wo)] ] ++ makeLists xs
    where 
       f (n,w) = ([n],w)

-- This is the original function.
-- combine the list of int of same words into one list.
amalgamate :: [ ([Int],Word1) ] -> [ ([Int],Word1) ]
amalgamate []   = []
amalgamate [p]  = [p]
amalgamate ((l1,w1):(l2,w2):rest)
    | w1 /= w2  = (l1,w1) : amalgamate ((l2,w2):rest)
    | otherwise = amalgamate ((l1++l2,w1):rest)

--This function is modified according to 12.31- lambda expression.
--remove all short < 3 words.
shorten :: [([Int],Word1)] -> [([Int],Word1)]
shorten = filter (\(nl,wd) -> ((length wd) > 3))

-- This is original function.
-- call makeindex function.
makeIndex :: Doc -> [ ([Int],Word1) ]
makeIndex
  = lines       >.>    --   Doc            -> [Line]
    numLines    >.>     --   [Line]         -> [(Int,Line)] 
    allNumWords >.>     --   [(Int,Line)]   -> [(Int,Word)]
    sortLs orderPair     >.>     --   [(Int,Word)]   -> [(Int,Word)]
    makeLists   >.>     --   [(Int,Word)]   -> [([Int],Word)]
    amalgamate  >.>     --   [([Int],Word)] -> [([Int],Word)]
    shorten             --   [([Int],Word)] -> [([Int],Word)]
	
-- This is for >.> mention in makeIndex.
infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g
