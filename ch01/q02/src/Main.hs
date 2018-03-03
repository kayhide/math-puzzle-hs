module Main where

import ClassyPrelude

numbers :: [Int]
numbers = [1000 .. 9999]

-- |
-- >>> split 123
-- [[1,23],[12,3]]
split :: Int -> [[Int]]
split 123 = [[1, 23], [12, 3]]
split i = [[i]]

main :: IO ()
main = do
  putStrLn "hello world"
