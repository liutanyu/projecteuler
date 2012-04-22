import Data.List (sort)

productList n = [a*b | a <- [100, 101..n], b <- [100, 101..n]]

palindromic [] = []
palindromic (x:xs)
  | show x == reverse (show x) = x:palindromic xs
  | otherwise = palindromic xs


result = last.sort.palindromic.productList
