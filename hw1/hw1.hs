toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft []       = []
doubleFromLeft [x]      = [x]
doubleFromLeft (x:y:xs) = x : (2*y) : doubleFromLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleFromLeft (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 start goal _    = [(start, goal)]
hanoi n start goal temp = hanoi (n-1) start temp goal ++
                          hanoi 1 start goal temp ++
                          hanoi (n-1) temp goal start

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 start goal _  _  = [(start, goal)]
hanoi4 2 start goal t1 _  = [(start, t1), (start, goal), (t1, goal)]
hanoi4 n start goal t1 t2 = hanoi4 (n-2) start t1 goal t2 ++
                            hanoi4 2 start goal t2 t1 ++
                            hanoi4 (n-2) t1 goal start t2