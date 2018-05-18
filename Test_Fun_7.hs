module Test_Fun_7
where

import Fun_7_module_segment
import Fun_7_module_point

main =
    do 
					putStrLn ("showseg ((1,1),(2,2)) = " ++ show(showseg((1,1),(2,2))))
					putStrLn ("midpt ((1,1),(2,2)) = " ++ show(midpt((1,1),(2,2))))
					putStrLn ("showseg ((1.5,8.5),(2.5,2)) = " ++ show(showseg((1.5,8.5),(2.5,2))))
					putStrLn ("midpt ((1.5,8.5),(2.5,2)) = " ++ show(midpt((1.5,8.5),(2.5,2))))
