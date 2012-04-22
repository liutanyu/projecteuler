isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
            where s = floor $ sqrt $ fromIntegral n

factor n = reverse $ filter isPrime [1..s]
           where s = floor $ sqrt $ fromIntegral n
                 
result n = head [x | x <- factor n, n `mod` x == 0]