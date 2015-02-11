{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}
-- First option is needed because we didn't bother implementing
-- all of the methods of the Num class
-----------------Exercise 1-----------------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]
-----------------Exercise 2-----------------
fibs2 :: [Integer]
fibs2 = [0, 1] ++ [fibs2!!(n-1) + fibs2!!(n-2) | n <- [2..]]
-- Can also do it with map
fibs2' :: [Integer]
fibs2' = [0, 1] ++ (map f [2..])
  where f n = fibs2'!!(n-1) + fibs2'!!(n-2)
-----------------Exercise 3-----------------
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : (streamToList s)

instance Show a => Show (Stream a) where
  show s = "Stream " ++ (show . take 20 $ streamToList s)
-----------------Exercise 4-----------------
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)
-----------------Exercise 5-----------------
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap log2 $ streamFromSeed (+1) 1
  where log2 n | even n    = 1 + (log2 $ n `div` 2)
               | otherwise = 0
-----------------Exercise 6-----------------
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n                 = Cons n $ streamRepeat 0
  negate (Cons c s)             = Cons (-c) $ negate s
  (Cons c s) + (Cons c' s')     = Cons (c + c') $ s + s'
  -- Could define this more "naturally" by using (fromInteger a0)*b' instead of a0b'
  -- but this way is significantly more efficient
  (Cons a0 a') * b@(Cons b0 b') = Cons (a0*b0) $ a0b' + a'*b
                                    where a0b' = streamMap (*a0) b'

instance Fractional (Stream Integer) where
  -- using integer division for coefficients
  (Cons a0 a') / (Cons b0 b') = q
    where q       = Cons (a0 `div` b0) $ a'divb0 - q*b'
          a'divb0 = streamMap (`div` b0) a'

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
-----------------Exercise 6-----------------
data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
  show (Matrix a b c d) = "Matrix " ++ show ((a, b), (c,d))

instance Num Matrix where
  fromInteger n           = Matrix n 0
                                   0 n
  negate (Matrix a b c d) = Matrix (-a) (-b)
                                   (-c) (-d)
  (Matrix a b c d) + (Matrix a' b' c' d') = Matrix (a+a') (b+b')
                                                   (c+c') (d+d')
  (Matrix a b c d) * (Matrix a' b' c' d') = Matrix (a*a' + b*c') (a*b' + b*d')
                                                   (c*a' + d*c') (c*b' + d*d')

fib4 :: Integer -> Integer
fib4 0 = 0
-- For some reason, getA fibMat^n is the same as (getA fibMat)^n
fib4 n = getA $ fibMat^n
  where fibMat = Matrix 1 1
                        1 0
        getA (Matrix a _ _ _) = a
