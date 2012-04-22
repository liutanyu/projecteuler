fib a b n 
  | n == 0 = 0
  | n == 1 = a
  | n == 2 = b
  | otherwise = fib b (a+b) (n-1)
            
fibnacci = fib 1 2

list = filter even $ takeWhile (<4000000) $ map fibnacci [1..]

result = sum list