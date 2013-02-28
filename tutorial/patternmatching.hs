capital :: String -> String
capital [] = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
 
getInitials :: String -> String -> String
getInitials firstname lastname = [f] ++ [l]
    where (f:_) = firstname
          (l:_) = lastname
 
main = do
    let myvar = "test"
    putStrLn $ (capital myvar)
    putStrLn $ (getInitials "John" "Smith")
