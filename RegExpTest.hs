

module RegExpTest
where
import RegExp


main =
    do
        putStrLn ("qustion 12.14 : regular expression is : star((a ||| b) <~> (a ||| b)) ")
        putStrLn ("for \"aa\" : " ++ show(star ((a ||| b) <~> (a ||| b)) "aa"))
        putStrLn ("for \"ababab\" : " ++ show(star ((a ||| b) <~> (a ||| b)) "ababab"))
        putStrLn ("for \"bbb\" : " ++ show(star ((a ||| b) <~> (a ||| b)) "bbb"))
        putStrLn ("for \"a\" : " ++ show(star ((a ||| b) <~> (a ||| b)) "a"))
        putStrLn ("for \" \" : " ++ show(star ((a ||| b) <~> (a ||| b)) ""))


        putStrLn ("qustion 12.15 : regular expression is : star(star((a ||| b) <~> (a ||| b))) ")
        putStrLn ("for \"aa\" : " ++ show(star ((a ||| b) <~> (a ||| b)) "aa"))
        putStrLn ("for \"ababab\" : " ++ show(star ((a ||| b) <~> (a ||| b)) "ababab"))
        putStrLn ("for \"bbb\" : " ++ show(star ((a ||| b) <~> (a ||| b)) "bbb"))
        putStrLn ("for \"a\" : " ++ show(star ((a ||| b) <~> (a ||| b)) "a"))
        putStrLn ("for \" \" : " ++ show(star ((a ||| b) <~> (a ||| b)) ""))
		
		
        putStrLn ("qustion 12.16 : plus function  ")
        putStrLn ("for  plus(a) \"aab\" : " ++ show(plus(a) "aab"))
        putStrLn ("for  plus(a) \"aaa\" : " ++ show(plus(a) "aaa"))
        putStrLn ("for  plus(a) \" \" : " ++ show(plus(a) ""))
        putStrLn ("for  plus(a) \"a\" : " ++ show(plus(a) "a"))
		
        putStrLn ("qustion 12.16 : option function  ")
        putStrLn ("for  option(a) \"aab\" : " ++ show(option(a) "aab"))
        putStrLn ("for  option(a) \"aaa\" : " ++ show(option(a) "aaa"))
        putStrLn ("for  option(a) \"\" : " ++ show(option(a) ""))
        putStrLn ("for  option(a) \"a\" : " ++ show(option(a) "a"))


        putStrLn ("qustion 12.17a : regular expression is : ((range '1' '9')<~>(star(range '0' '9')))  ")
        putStrLn ("for \" 10\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))) " 10"))
        putStrLn ("for \"01\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))) "01"))
        putStrLn ("for \"10\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))) "10"))
        putStrLn ("for \"11\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))) "11"))
        putStrLn ("for \"10 \" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))) "10 "))

        putStrLn ("qustion 12.17b : regular expression is : ((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9'))  ")
        putStrLn ("for \"14\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "14"))
        putStrLn ("for \"14.\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "14."))
        putStrLn ("for \"014.15\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "014.15"))
        putStrLn ("for \"14.50\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "14.50"))
        putStrLn ("for \"14.5\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "14.5"))
        putStrLn ("for \"14.05\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "14.05"))
        putStrLn ("for \"10.5\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "10.5"))
        putStrLn ("for \" 14.5\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) " 14.5"))
        putStrLn ("for \"14.5 \" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "14.5 "))
        putStrLn ("for \"14.5a\" : " ++ show(((range '1' '9')<~>(star(range '0' '9'))<~>dot<~>(star(range '0' '9'))<~>(range '1' '9')) "14.5a"))
       
        putStrLn ("qustion 12.18a : regular expression is : ((star b)<~>(a ||| epsilon)<~>(star b)<~>(a ||| epsilon)<~>(star b))  ")
        putStrLn ("for \" \" : " ++ show(((star b)<~>(a ||| epsilon)<~>(star b)<~>(a ||| epsilon)<~>(star b)) ""))
        putStrLn ("for \"a\" : " ++ show(((star b)<~>(a ||| epsilon)<~>(star b)<~>(a ||| epsilon)<~>(star b)) "a"))
        putStrLn ("for \"aa\" : " ++ show(((star b)<~>(a ||| epsilon)<~>(star b)<~>(a ||| epsilon)<~>(star b)) "aa"))
        putStrLn ("for \"aaa\" : " ++ show(((star b)<~>(a ||| epsilon)<~>(star b)<~>(a ||| epsilon)<~>(star b)) "aaa"))
        putStrLn ("for \"bbb\" : " ++ show(((star b)<~>(a ||| epsilon)<~>(star b)<~>(a ||| epsilon)<~>(star b)) "bbb"))
        putStrLn ("for \"babba\" : " ++ show(((star b)<~>(a ||| epsilon)<~>(star b)<~>(a ||| epsilon)<~>(star b)) "babba"))
		
        putStrLn ("qustion 12.18b : regular expression is : ((star b)<~>a<~>(star b)<~>a<~>(star b))  ")
        putStrLn ("for \" \" : " ++ show(((star b)<~>a<~>(star b)<~>a<~>(star b)) ""))
		

        putStrLn ("for \"a\" : " ++ show(((star b)<~>a<~>(star b)<~>a<~>(star b)) "a"))
        putStrLn ("for \"aa\" : " ++ show(((star b)<~>a<~>(star b)<~>a<~>(star b)) "aa"))
        putStrLn ("for \"ababa\" : " ++ show(((star b)<~>a<~>(star b)<~>a<~>(star b)) "ababa"))
        putStrLn ("for \"bbb\" : " ++ show(((star b)<~>a<~>(star b)<~>a<~>(star b)) "bbb"))
        putStrLn ("qustion 12.18c : regular expression is : (epsilon ||| (a ||| b) ||| ((a ||| b) <~> (a |||  b)) ||| ((a ||| b) <~> (a |||  b) <~> (a ||| b))) ")
        putStrLn ("for \" \" : " ++ show((epsilon ||| (a ||| b) ||| ((a ||| b) <~> (a |||  b)) ||| ((a ||| b) <~> (a |||  b) <~> (a ||| b))) ""))


        putStrLn ("for \"a\" : " ++ show((epsilon ||| (a ||| b) ||| ((a ||| b) <~> (a |||  b)) ||| ((a ||| b) <~> (a |||  b) <~> (a ||| b))) "a"))
        putStrLn ("for \"bb\" : " ++ show((epsilon ||| (a ||| b) ||| ((a ||| b) <~> (a |||  b)) ||| ((a ||| b) <~> (a |||  b) <~> (a ||| b))) "bb"))
        putStrLn ("for \"aba\" : " ++ show((epsilon ||| (a ||| b) ||| ((a ||| b) <~> (a |||  b)) ||| ((a ||| b) <~> (a |||  b) <~> (a ||| b))) "aba"))
        putStrLn ("for \"abaab\" : " ++ show((epsilon ||| (a ||| b) ||| ((a ||| b) <~> (a |||  b)) ||| ((a ||| b) <~> (a |||  b) <~> (a ||| b))) "abaab"))
        putStrLn ("qustion 12.18d : regular expression is : (star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon)  ")
        putStrLn ("for \" \" : " ++ show((star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon) ""))

        putStrLn ("for \"a\" : " ++ show((star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon) "a"))
        putStrLn ("for \"aa\" : " ++ show((star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon) "aa"))
        putStrLn ("for \"aba\" : " ++ show((star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon) "aba"))
        
        
        putStrLn ("for \"abab\" : " ++ show((star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon) "abab"))

        putStrLn ("for \"abba\" : " ++ show((star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon) "abba"))
        putStrLn ("for \"ab\" : " ++ show((star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon) "ab"))
        putStrLn ("for \"abaa\" : " ++ show((star(a <~> b <~> (a ||| epsilon)) ||| star(b <~> a <~> (b ||| epsilon))||| a ||| b ||| epsilon) "abaa"))
        




		
        


