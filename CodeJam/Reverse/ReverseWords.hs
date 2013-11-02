module ReverseWords (reverseWords)
where

reverseWords :: String -> String
reverseWords = unwords . reverse . words 
