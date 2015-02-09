module Golf where
import Data.Sequence (Seq, adjust, replicate)
import Data.Foldable (toList)
import Data.List ((\\))

--VERSION 1------------------------------------------------------
-- Get every nth element of the input list
-- n = 1 means get every element, n = 2 means get ever other
-- cur is a number used only for bookkeeping to make recursion easier
-- Should be called like nthElts 1 n xs
nthElts :: Int -> Int -> [a] -> [a]
nthElts _ _ []       = []
nthElts cur n (x:xs) = if cur == n
                       then x : (nthElts 1 n xs)
                       else nthElts (cur+1) n xs

-- The map expression creates a list of functions that get every nth
-- element from a list, where n ranges from 1 to the length of the
-- input list
-- Say that fs is that list of functions
-- zipWith id fs xs returns a list of the results from calculating
-- id f x (same as just f x) for every pair of elements from the lists
-- repeat xs just creates a list [xs, xs, xs, ...] so we can be sure
-- that zipping will work properly
skips :: [a] -> [[a]]
skips xs = zipWith id (map (nthElts 1) [1..(length xs)]) (repeat xs)
--VERSION 2----------------------------------------------------
nthElts2 :: [a] -> Int -> Int -> [a]
nthElts2 [] _ _ = []
nthElts2 (x:xs) cur n = if cur == n
                       then x : (nthElts2 xs 1 n)
                       else nthElts2 xs (cur+1) n

skips2 :: [a] -> [[a]]
skips2 xs = map (nthElts2 xs 1) [1..(length xs)]
----------------------------------------------------------------
localMaxima :: [Integer] -> [Integer]
localMaxima []         = []
localMaxima [_]        = []
localMaxima [_,_]      = []
localMaxima (x:y:z:xs) = if y > x && y > z
                         then y : rest
                         else rest
  where rest = localMaxima (y:z:xs)
---------------------VERSION 1----------------------
-- for each i in xs, increment the value at position i in s
acc :: Seq Int -> [Int] -> Seq Int
acc s []     = s
acc s (x:xs) = acc (adjust (+1) x s) xs

-- sequence of 10 zeroes
zeroSeq :: Seq Int
zeroSeq = Data.Sequence.replicate 10 0

-- get a string representing a column of the histogram
-- m is the max size of any histogram bar
-- i is the index (the label at the bottom of the bar)
-- c is the height of the histogram bar
getCol :: Int -> (Int, Int) -> String
getCol m (i,c) = Prelude.replicate (m-c) ' ' ++
                 Prelude.replicate c '*' ++
                 ('=': show i)

-- get list of all column strings for the given number frequencies
getCols :: [Int] -> [String]
getCols xs = map (getCol m) (zip [0..] xs)
  where m = maximum xs

-- convert a list of column strings into a list of row strings
-- input list must have 10 elements
joinCols :: [String] -> [String]
joinCols xs
  | head xs == [] = []
  | otherwise     = (map head xs) : joinCols (map tail xs)

-- get list of row strings, then join then with newlines
histogram :: [Integer] -> String
histogram xs = unlines $ joinCols . getCols $ toList $ acc zeroSeq ys
  where ys = map fromInteger xs

---------------------VERSION 2----------------------
getFirstLine :: [Integer] -> String
getFirstLine xs = map getSym [0..9]
  where getSym i | i `elem` xs = '*'
                 | otherwise   = ' '

getHistoList :: [Integer] -> [String]
getHistoList [] = []
getHistoList xs = getFirstLine xs : (getHistoList $ xs \\ [0..9])

histogram2 :: [Integer] -> String
histogram2 xs = (unlines . reverse $ getHistoList xs) ++ "==========\n0123456789"