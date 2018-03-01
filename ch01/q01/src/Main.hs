module Main where

import           ClassyPrelude

import qualified Data.Text             as T
import           Formatting            (sformat)
import           Formatting.Formatters (bin, int, oct)


numbers :: [Int]
numbers = [ 10 .. ]

isPalindrome :: Text -> Bool
isPalindrome t = t == T.reverse t

isPalindromeNumber :: Int -> Bool
isPalindromeNumber n =
  isPalindrome (sformat bin n) &&
  isPalindrome (sformat oct n) &&
  isPalindrome (sformat int n)

main :: IO ()
main = do
  let x = headMay $ filter isPalindromeNumber numbers
  print x
