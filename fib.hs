fib :: Int -> Int
fib n | n == 0 = 1
      | n == 1 = 1
      | otherwise = fib (n-1) + fib(n-2)

main = print (fib 10)
