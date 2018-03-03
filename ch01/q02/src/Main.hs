module Main where

import ClassyPrelude

import Data.List (unfoldr)

numbers :: [Int]
numbers = [1000 .. 9999]

-- |
-- >>> split 1
-- [1]
-- >>> split 123
-- [1,2,3]
-- >>> split 1020
-- [1,0,2,0]
-- >>> split (-12)
-- [1,2]
split :: Int -> [Int]
split i = reverse $ unfoldr f $ abs i
  where
    f 0 = Nothing
    f x = Just $ swap $ divMod x 10

-- |
-- >>> combinations [1, 2]
-- [[1,2],[12]]
-- >>> combinations [1, 2, 3]
-- [[1,2,3],[1,23],[12,3],[123]]
-- >>> combinations [1, 0, 2]
-- [[1,0,2],[1,2],[10,2],[102]]
combinations :: [Int] -> [[Int]]
combinations [] = [[]]
combinations [x] = [[x]]
combinations (x : xs@(y : ys)) = ((x :) <$> combinations xs) <> combinations (x * 10 + y : ys)

data Factor = Val Int | Prod Factor Factor | Div Factor Factor
  deriving (Show)
data Term = Term Factor | Sum Term Term | Sub Term Term
  deriving (Show)


class Format a where
  format :: a -> Text

-- |
-- >>> format $ Div (Prod (Val 7) (Val 6)) (Val 2)
-- "7 * 6 / 2"
instance Format Factor where
  format (Val i) = tshow i
  format (Prod x y) = format x <> " * " <> format y
  format (Div x y) = format x <> " / " <> format y

-- |
-- >>> format $ Sum (Term (Prod (Val 7) (Val 2))) (Sub (Term (Val 8)) (Term (Div (Val 6) (Val 3))))
-- "7 * 2 + 8 - 6 / 3"
instance Format Term where
  format (Term x) = format x
  format (Sum x y) = format x <> " + " <> format y
  format (Sub x y) = format x <> " - " <> format y


class Eval a where
  eval :: a -> Maybe Int

-- |
-- >>> eval $ Prod (Val 7) (Val 6)
-- Just 42
-- >>> eval $ Div (Val 42) (Val 2)
-- Just 21
-- >>> eval $ Div (Val 42) (Val 0)
-- Nothing
instance Eval Factor where
  eval (Val i) = Just i
  eval (Prod x y) = (*) <$> eval x <*> eval y
  eval (Div x y) = div <$> eval x <*> verifyNonZero (eval y)
    where
      verifyNonZero (Just 0) = Nothing
      verifyNonZero v = v

-- |
-- >>> eval $ Sum (Term (Prod (Val 7) (Val 2))) (Sub (Term (Val 8)) (Term (Div (Val 6) (Val 3))))
-- Just 20
instance Eval Term where
  eval (Term x) = eval x
  eval (Sum x y) = (+) <$> eval x <*> eval y
  eval (Sub x y) = (-) <$> eval x <*> eval y


-- |
-- >>> format <$> build 34
-- ["3 + 4","3 - 4","3 * 4","3 / 4"]
build :: Int -> [Term]
build i = do
  ns <- filter ((> 1) . length ) $ combinations $ split i
  fs <- buildFactors $ Val <$> ns
  buildTerms $ Term <$> fs

-- |
-- >>> fmap format <$> buildFactors [Val 1, Val 2, Val 3]
-- [["1","2","3"],["1","2 * 3"],["1","2 / 3"],["1 * 2","3"],["1 * 2 * 3"],["1 * 2 / 3"],["1 / 2","3"],["1 / 2 * 3"],["1 / 2 / 3"]]
buildFactors :: [Factor] -> [[Factor]]
buildFactors [] = [[]]
buildFactors [x] = [[x]]
buildFactors (x : xs@(y : ys)) =
  ((x :) <$> buildFactors xs) <>
  buildFactors (Prod x y : ys) <>
  buildFactors (Div x y : ys)

-- |
-- >>> format <$> buildTerms [Term (Val 1), Term (Val 2), Term (Val 3)]
-- ["1 + 2 + 3","1 + 2 - 3","1 - 2 + 3","1 - 2 - 3"]
buildTerms :: [Term] -> [Term]
buildTerms [] = []
buildTerms [x] = [x]
buildTerms (x : xs) = (Sum x <$> buildTerms xs) <> (Sub x <$> buildTerms xs)


main :: IO ()
main = do
  traverse_ print' $ catMaybes $ go <$> (build' =<< numbers)
  putStrLn ""
  where
    build' i = (,) i <$> build i
    go (i, t) = do
      res <- eval t
      guard $ tshow i == reverse (tshow res)
      pure (i, t, res)
    print' (i, t, res) =
      putStrLn $ tshow i <> " (" <> format t <> " = " <> tshow res <> ")"
