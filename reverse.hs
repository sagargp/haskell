rev :: [a] -> [a]           -- "constructor"
rev []     = []             -- base case
rev (x:xs) = rev xs ++ [x]  -- recursion

main       = print (rev [1, 2, 3, 4])
