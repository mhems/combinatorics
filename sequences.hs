factorial :: Integral a => a -> a
factorial n
  | n <= 1    = n
  | otherwise = (*n) . factorial $ n-1

fibonacci :: Int -> Integer
fibonacci n = fibonacciSeq !! n

fibonacciSeq :: [Integer]
fibonacciSeq = 1 : 1 : [ f | f <- zipWith (+) fibonacciSeq $ tail fibonacciSeq ]

binomial :: Integral a => a -> a -> a
binomial n k
  | k == 0 = 1
  | k == n = 1
  | otherwise = (binomial (n-1) (k-1)) + (binomial (n-1) k)

binomialSeq :: Integral a => a -> [a]
binomialSeq n = [ binomial n k | k <- [0..n] ]

sequify :: (Enum a, Num a) => (a -> b) -> [b]
sequify f = map f [0..]

catalan :: Integral a => a -> a
catalan n
  | n == 0 = 1
  | otherwise = sum [ (catalan i) * (catalan (n-i-1)) | i <- [0..(n-1)] ]

catalanSeq :: [Integer]
catalanSeq = sequify catalan

bell :: Integral a => a -> a
bell n
  | n == 0 = 1
  | otherwise = sum [ (binomial (n-1) k) * (bell k) | k <- [0..(n-1)] ]

bellSeq :: [Integer]
bellSeq = sequify bell

derangement :: Integral a => a -> a
derangement n
  | n == 0 = 1
  | odd n     = -1 + n * derangement (n-1)
  | otherwise =  1 + n * derangement (n-1)

derangementSeq :: [Integer]
derangementSeq = sequify derangement

totient :: Integral a => a -> a
totient n = n * a `quot` b
  where a = product $ map (subtract 1) $ filter isPrime $ filter (\x -> n `mod` x == 0) [2..n]
        b = product $ filter isPrime $ factors n

totientSeq :: [Integer]
totientSeq = sequify totient

factors :: Integral a => a -> [a]
factors n = filter (\x -> n `mod` x == 0) [1..n]

factorization :: Integral a => a -> [(a,a)]
factorization n = [ (p, power n p) | p <- filter isPrime (factors n) ]
  where power n p
          | n `mod` p == 0 = 1 + power (n `quot` p) p
          | otherwise = 0

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1, n]

isPrimePower :: Integral a => a -> Bool
isPrimePower n = 1 == length (filter isPrime (factors n))
