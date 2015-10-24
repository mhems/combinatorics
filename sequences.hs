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

totient :: Integral a => a -> a
totient n = n * a `quot` b
  where a = product $ map (\x -> x-1) (filter isPrime (filter (\x -> n `mod` x == 0) [2..n]))
        b = product $ filter isPrime $ factors n

totientSeq :: [Integer]
totientSeq = [ totient i | i <- [0..] ]

factors :: Integral a => a -> [a]
factors n = [ i | i <- [1..n], n `mod` i == 0 ]

factorization :: Integral a => a -> [(a,a)]
factorization n = [ (p, power n p) | p <- filter isPrime (factors n) ]
  where power n p
          | n `mod` p == 0 = 1 + power (n `quot` p) p
          | otherwise = 0

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1, n]

isPrimePower :: Integral a => a -> Bool
isPrimePower n = 1 == length (filter isPrime (factors n))
