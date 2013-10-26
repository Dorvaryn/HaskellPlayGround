module ReverseWords (reverseWords)
where

import Data.List

reverseWords :: String -> String
reverseWords = unwords . reverse . words 
