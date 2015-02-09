import Data.List ((\\))

---------------EXERCISE 1---------------
fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (-2 +) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate f
  where f n | even n    = n `div` 2
            | otherwise = 3 * n + 1

---------------EXERCISE 2---------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = 0
getHeight (Node h _ _ _) = h

insert :: Tree a -> a -> Tree a
insert Leaf x = Node 1 Leaf x Leaf
insert (Node height left val right) x = if getHeight left <= getHeight right
                                        then let nl = insert left x
                                                 h = 1 + (maximum $ map getHeight [nl, right])
                                             in Node h nl val right
                                        else let nr = insert right x
                                             in Node height left val nr

foldTree :: [a] -> Tree a
foldTree = foldl insert Leaf

---------------EXERCISE 3---------------
-- straight from the definition, but no fold
xor' :: [Bool] -> Bool
xor' = not . even . length . filter (==True)

xor :: [Bool] -> Bool
xor = foldl oneXor False
  where oneXor x y = not $ x == y

map' :: (a -> b) -> [a] -> [b]
map' f = foldr g []
  where g x ys = (f x) : ys

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr g base ys
  where g  = flip f
        ys = reverse xs

---------------EXERCISE 4---------------
-- Doesn't really use function composition
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map (\x -> 2*x + 1) $ [1..n] \\ crossedOut'
  where f i j = i + j + 2*i*j
        crossedOut' = [f i j | i <- [1..n], j <- [1..n], i <= j, f i j <= n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ [1..n] \\ crossedOut n

crossedOut :: Integer -> [Integer]
crossedOut n = filter (<=n) . map (\(i,j) -> i + j + 2*i*j) . filter (\(i,j) -> i <= j) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]