-- realy slow!

import Data.List (sort, group)

isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
            where s = floor $ sqrt $ fromIntegral n

nextPrime n = if isPrime (n+1) then
                n+1
              else 
                nextPrime (n+1)
                
primeList' :: (Integral a) => a -> [a] -> [a]                
primeList' a lis = if a < last lis then
                     lis
                   else
                     primeList' a (sort (nextPrime (last lis) : lis))
                    
primeFactors 1 = [1]
primeFactors n = [x | x <- primeList' n [2], n `mod` x == 0]



factorization n = let plist = primeFactors n in
  factorization' n plist


factorization' _ [] = []
--factorization' 1 _ = [1]
factorization' n (x:xs) = if n `mod` x == 0 then
                            x:factorization' (n `div` x) (x:xs)
                          else factorization' n xs
         

factors n = map factorization [2,3..n]
  
result' [] a = a
result' b [] = b
result' list r = if (head $ head $ z list) < (head $ head $ z r) then
                   head list ++ result' (dz $ tail $ z list) r
                 else if head.head.z list > head.head.z r then
                        result' (dz.z list) (dz.tail.z r)
                      else if length.head.head.z list > length.head.head.z r then
                             dz.head.z list ++ result' (dz.tail.z list) (dz.tail.z r)
                           else  result' (dz.z list) (dz.tail.z r)
  where z n = group $ sort n
        dz n = sort $ concat n

result n =  foldr result' [] (factors n)