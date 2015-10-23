factorial :: Integral a => a -> a
factorial n
  | n <= 1    = n
  | otherwise = n * (factorial (n-1))

fibonacci :: Integral a => a -> a
fibonacci n
  | n <= 1    = n
  | otherwise = (fibonacci (n-1)) + (fibonacci (n-2))

fibonacciSeq :: [Integer]
fibonacciSeq = [ fibonacci x | x <- [0..] ]

binomial :: Integral a => a -> a -> a
binomial n k
  | k == 0 = 1
  | k == n = 1
  | otherwise = (binomial (n-1) (k-1)) + (binomial (n-1) k)

binomialSeq :: Integral a => a -> [a]
binomialSeq n = [ binomial n k | k <- [0..n] ]

catalan :: Integral a => a -> a
catalan n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = sum [ (catalan i) * (catalan (n-i-1)) | i <- [0..(n-1)] ]

catalanSeq :: [Integer]
catalanSeq = [ catalan n | n <- [0..] ]

bell :: Integral a => a -> a
bell n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = sum [ (binomial (n-1) k) * (bell k) | k <- [0..(n-1)] ]

bellSeq :: [Integer]
bellSeq = [ bell i | i <- [0..] ]

derangement :: Integral a => a -> a
derangement n
  | n == 0 = 1
  | n == 1 = 0
  | odd n     = -1 + n * derangement (n-1)
  | otherwise =  1 + n * derangement (n-1)

derangementSeq :: [Integer]
derangementSeq = [ derangement i | i <- [0..] ]

-- fix
totient :: Integral a => a -> a
totient n = n * product [ 1 - (1 `div` p) | p <- [2..n], n `mod` p == 0 ]

factors :: Integral a => a -> [a]
factors n = [ i | i <- [1..n], n `mod` i == 0 ]

-- fix
--factorization :: Integral a => a -> [a]
factorization n = [7]

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1, n]

-- fix
--isPrimePower :: Integral a => a -> Bool
--isPrimePower n = 1 == length $ factorization n
