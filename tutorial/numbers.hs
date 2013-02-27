
main = do
    putStrLn "Input a number:";
    a <- readLn

    let b = fact a
    let c = fib a
    let d = hanoi a "p1" "p2" "p3"
    
    putStr "Factorial: "; print b
    putStr "Fibonacci: "; print c
    putStrLn "Hanoi: "
    putStr d

-- fact
fact:: Int -> Int
fact 0 = 1
fact n = n*fact(n-1)

-- fib
fib:: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = (fib(n-1))+(fib(n-2))

-- hanoi
hanoi:: Int -> String -> String -> String -> String
hanoi 1 p1 p2 p3 = "From " ++ p1 ++ " to " ++ p2 ++ "\n"
hanoi n p1 p2 p3 = (hanoi (n-1) p1 p3 p2) ++ (hanoi 1 p1 p2 p3) ++ (hanoi (n-1) p3 p2 p1)
