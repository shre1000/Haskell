-- CSCI 556: Multiparadigm Programming 
-- Assignment #3, Indexing
-- Shreyasi Kokamthankar
-- Student ID: 10599411

-- This is source code module.
module Demo
(Doc, Line, Word1, lines1, numLines,numWords, whitespace, splitWords, dropSpace, orderPair1,
split, getWord, dropWord, allNumWords, sortLs, orderPair, makeLists, amalgamate, shorten, makeIndex,modifyCapEqui, modifyCapEqui_Intermediate)
where

-- add orderPair1
import  Data.Char
type Doc = String
type Line = String
type Word1 = String 

-- take input document and split it line wise by \n 
--lines :: Doc -> [Line]
-- we are using default lines function in prelude.
lines1 :: Doc -> [Line]
lines1 d 
         |d /= [] =  [takeWhile (/= '\n') d] ++ lines1 (drop 1 (dropWhile (/= '\n') d))
		 |otherwise = [] 

modifyCapEqui :: [Line] -> [Line]
modifyCapEqui xs = map modifyCapEqui_Intermediate xs

modifyCapEqui_Intermediate :: String -> String
modifyCapEqui_Intermediate = map toLower 
		 
-- step 1 :  pair each line with its line no.
numLines :: [Line] -> [(Int , Line)]
numLines l = zip [1 .. length l] l

-- step 2 : part a: split line into words and assign word a corresponding line number. this is for single line. 
--numWords (number, line) = map (\word -> (number, word)) (splitWords line)
numWords :: (Int , Line) -> [(Int , Word1)]
numWords (number , line) = [ (number , word) | word <- splitWords line ]

-- whitespace declairation.
whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"

-- following  set of functions are use by numWords.

-- split the string

-- we are using splitWords because there may be space before the first word of string so we will remove it.
splitWords :: String -> [Word1]
splitWords st = split (dropSpace st)

-- this will split string into words.
split :: String -> [Word1]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

--drop the whitespace character from the string 
dropSpace :: String -> String
dropSpace []              = []
dropSpace (x:xs) 
    | elem x whitespace   = dropSpace xs
    | otherwise           = (x:xs)

-- get the front word of string
getWord :: String -> String
getWord []                = [] 
getWord (x:xs) 
    | elem x whitespace   = []
    | otherwise           = x : getWord xs

-- drop the front words of string	
dropWord :: String -> String
dropWord []               = []
dropWord (x:xs) 
    | elem x whitespace   = (x:xs)
    | otherwise           = dropWord xs

-- end of functions use by numWords

-- step 2 : part b: do it for all lines.
allNumWords :: [( Int , Line)] -> [(Int , Word1)]
allNumWords  = concat . map numWords 

{-- Step 3 : sorting the list.
-- original function
sortLs :: [(Int , Word1)] -> [(Int , Word1)]
sortLs []     = []
sortLs (p:ps) = sortLs smaller ++ [p] ++ sortLs larger
    where 
      smaller = [ q | q<-ps , orderPair q p ]
      larger  = [ q | q<-ps , orderPair p q ]

--}
{-- function for duplicate elements.
sortLs :: [(Int , Word1)] -> [(Int , Word1)]
sortLs []     = []
sortLs (p:ps) = sortLs smaller ++ [p] ++ sortLs equal ++ sortLs larger
    where 
      smaller = [ q | q<-ps , orderPair q p ]
      larger  = [ q | q<-ps , orderPair p q ]
      equal   = [ q | q<-ps , orderPair1 p q ]
	  
--}	  
{-- ordering is used for sorting.		
orderPair :: (Int , Word1) -> (Int , Word1) -> Bool
orderPair ( n1 , w1 ) ( n2 , w2 ) = w1 < w2 || ( w1 == w2 && n1 < n2 )
--}
{-- new function to maintain duplicate elements.
orderPair1 :: (Int , Word1) -> (Int , Word1) -> Bool
orderPair1 ( n1 , w1 ) ( n2 , w2 ) = w1 < w2 || ( w1 == w2 && n1 == n2 )
--}

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
-- step 4 : make list of int.

--original function
makeLists ::  [ (Int,Word1) ] -> [ ([Int],Word1) ]
makeLists = map mklis 
    where
        mklis ( n , st ) = ( [n] , st )

--}


{--
 lambda function
makeLists ::  [ (Int,Word1) ] -> [ ([Int],Word1) ]
makeLists = map (\(n,st) -> ([n],st))
--}

{-- list comprehension
makeLists ::  [(Int,Word1)] -> [([Int],Word1)]
makeLists [] = []
makeLists ((no,wo):xs) =  [f (n,w) | (n,w) <- [(no,wo)] ] ++ makeLists xs
    where 
       f (n,w) = ([n],w)
--}

-- step 5 : combine the list of int of same words into one list.
amalgamate :: [ ([Int],Word1) ] -> [ ([Int],Word1) ]
amalgamate []   = []
amalgamate [p]  = [p]
amalgamate ((l1,w1):(l2,w2):rest)
    | w1 /= w2  = (l1,w1) : amalgamate ((l2,w2):rest)
    | otherwise = amalgamate ((l1++l2,w1):rest)

--step 6: remove all short < 3 words.
--original function
shorten :: [([Int],Word1)] -> [([Int],Word1)]
shorten = filter sizer 
    where
        sizer (nl,wd) = length wd > 3

{-- lambda function
shorten :: [([Int],Word1)] -> [([Int],Word1)]
shorten = filter (\(nl,wd) -> ((length wd) > 3))
--}

{--list comprehension
shorten :: [([Int],Word1)] -> [([Int],Word1)]
shorten [] = []
shorten (([no],wo):xs) =  [sizer ([n],w) | ([n],w) <- [([no],wo)], (length w) > 3 ] ++ shorten xs
    where 
       sizer ([n],w) = ([n],w)

--}
		
-- step 7 : call makeindex function.
makeIndex :: Doc -> [ ([Int],Word1) ]
makeIndex
  = lines       >.>    --   Doc            -> [Line]
    modifyCapEqui >.>
    numLines    >.>     --   [Line]         -> [(Int,Line)] 
    allNumWords >.>     --   [(Int,Line)]   -> [(Int,Word)]
    sortLs      >.>     --   [(Int,Word)]   -> [(Int,Word)]
    makeLists   >.>     --   [(Int,Word)]   -> [([Int],Word)]
    amalgamate  >.>     --   [([Int],Word)] -> [([Int],Word)]
    shorten             --   [([Int],Word)] -> [([Int],Word)]
	
-- This is for >.> mention in makeIndex. dont need to mention it in () where.
infixl 9 >.>

(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g
