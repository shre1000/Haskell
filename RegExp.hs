

module RegExp
(RegExp, a, b, epsilon, char, (|||), (<~>), splits, (<**>), fsplits, star, plus, option, range, dot)
where

import Data.List (foldl1)

type RegExp = String -> Bool

a,b :: RegExp
a = char 'a'
b = char 'b'

dot :: RegExp
dot = char '.'

-- empty string
epsilon :: RegExp
epsilon = (=="")

-- single character
char :: Char -> RegExp
char ch = (==[ch])

-- or 
(|||) :: RegExp -> RegExp ->  RegExp
e1 ||| e2 = 
    \x -> e1 x || e2 x

-- and
(<~>) :: RegExp -> RegExp ->  RegExp
e1 <~> e2 = 
    \x -> or [ e1 y && e2 z | (y,z) <- splits x ]

-- function used in and
splits :: String -> [(String,String)]
splits xs = [splitAt n xs | n<-[0..len]]
    where
      len = length xs

-- modified and
(<**>) :: RegExp -> RegExp ->  RegExp
e1 <**> e2 =
    \x -> or [ e1 y && e2 z | (y,z) <- fsplits x ]

-- function used for modified and 
fsplits :: String -> [(String,String)]
fsplits xs = tail (splits xs)

-- * operation
star :: RegExp -> RegExp
star p = epsilon ||| (p <**> star p)
--           epsilon ||| (p <~> star p)
-- is OK as long as p can't have epsilon match

----------------------------------------      solution to all assignment questions starts here.     -----------------

-- 12.14: Regular expression star((a ||| b) <~> (a ||| b))  is matched by only those strings that have even number of letters(a's and b's) total.
-- Example of such strings are : "ababab", "aa", "bb" 

-- 12.15: Regular expression star(star((a ||| b) <~> (a ||| b)))  is matched by only those strings that have even number of letters(a's and b's) total.
-- Example of such strings are : "ababab", "aa", "bb" 

-- 12.16 : Define functions option and plus.

-- plus e matches one or more occurrences of pattern e
plus :: RegExp -> RegExp
plus e = e <**> star e

-- option e matches zero or one occurrences of the pattern e
option :: RegExp -> RegExp
option e = epsilon ||| e

-- 12.17 Define a regular expression which match
-- 1. Strings of digits which begin with a non - zero digit
-- example of such string "1" , "10"
-- ((range '1' '9')<~>(star(range '0' '9')))

-- 2. Fractional numbers : two strings of digits separated by '.'. Numbbers do not have superfluous zeros at the begining or at the end.
-- so exclude strings like "01.34" and "1.20".
-- examples: "12.5" , "15.05" , "10.05"
-- define function range for this.
-- ((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9'))

-- range function
range:: Char-> Char -> RegExp
range ch1 ch2 
    | ch1 == '1' = char '1' ||| char '2' ||| char '3' ||| char '4' ||| char '5' ||| char '6' ||| char '7' ||| char '8' ||| char '9' 
    | ch1 == '0' = char '0' ||| char '1' ||| char '2' ||| char '3' ||| char '4' ||| char '5' ||| char '6' ||| char '7' ||| char '8' ||| char '9'
   
   
-- 12.18 Give regular expressions describing the following sets of strings.
-- 1. All strings of a's and b's containing at most two a's.
-- ((star b)<~>(a ||| epsilon)<~>(star b)<~>(a ||| epsilon)<~>(star b))

-- 2. All the strings of a's and b's containing exactly two a's.
--  ((star b)<~>a<~>(star b)<~>a<~>(star b))

-- 3. All the string of a's and b's of length at most three.
-- (epsilon ||| (a ||| b) ||| ((a ||| b) <~> (a |||  b)) ||| ((a ||| b) <~> (a |||  b) <~> (a ||| b)))

-- 4. All strings of a's and b's which contain no repeated adjacent characters, that is no substring of the form aa or bb.
--  (star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon)

