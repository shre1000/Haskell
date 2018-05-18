-- Questions:
-- 3. Develop a Boolean function div23n5 such that div23n5 n returns True if and only if n is divisible by 2 or divisible by 3 but not divisible by 5. That is, div23n5 6 yields True and div23n5 30 yields False.

-- 5. Develop a function mult that takes two natural numbers and returns their product. The function must not use the multiplication (*) or division (div) operators. Hint: Multiplication can be done by repeated addition.

-- 6. Develop function addTax that takes two Double values such that addTax c p returns c with a sales tax of p percent added. For example, addTax 1.0 9.0 returns 1.09.

-- Also develop function subTax such that subTax (addTax c p) p returns c.

module Fun3_5_6
(mult, div23n5, addTax, subTax)
where


mult :: Int -> Int -> Int
mult  n1 n2 = if n1==0 then
                         0
                      else if n2==0 then
                          0
                       else
                           n1 + mult n1 (n2-1)
	
	
div23n5 :: Int -> Bool
div23n5 n = if ( ( ((n `mod` 2) ==0) || ((n `mod` 3)==0) ) && ((n `mod` 5) /= 0) ) then
                             True
                    else
                              False
							  
addTax :: Double -> Double -> Double
addTax c p = c+((c*p)/100)            

subTax :: Double -> Double -> Double 
subTax c p  =  c/(1+(p/100))
