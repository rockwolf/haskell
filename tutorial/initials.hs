
getInitials :: String -> String -> String
getInitials firstname lastname = [f] ++ [l]
    where (f:_) = firstname
          (l:_) = lastname
