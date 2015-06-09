module Palindrome where
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [x] = True
isPalindrome [] = False
isPalindrome (x:xs) = (x == last xs) && (isPalindrome (take ((length xs)-1) xs))

main = print (isPalindrome ["racecar"])
