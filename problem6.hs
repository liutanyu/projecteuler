sqrt' x= x^2

problem6 :: Int -> Int

problem6 x = 
  (sum [1..x])^2 - (sum $ map sqrt' [1..x])