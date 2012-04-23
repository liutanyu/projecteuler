-- realy slow!

import Data.List (sort)

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
                    
primeFactors n = [x | x <- primeList' n [2], n `mod` x == 0]

result n = last $ sort $ primeFactors n