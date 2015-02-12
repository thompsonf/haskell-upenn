{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)
-----------------Exercise 1-----------------
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m
-----------------Exercise 2-----------------
-- Very simple JoinList for testing
yeah :: JoinList Size Char
yeah = ((Single 1 'y') +++ ((Single 1 'e') +++ (Single 1 'a'))) +++ (Single 1 'h')

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jlTagAsInt :: (Sized b, Monoid b) => JoinList b a -> Int
jlTagAsInt l = getSize . size . tag $ l

-- It isn't strictly necessary to explicitly define handling for Empty
-- and Single (here or below) since they will always have size 0 and
-- size 1, respectively. -- However, I think it's clearer to have them here.
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty                        = Nothing
indexJ i _ | i < 0                    = Nothing
indexJ 0 (Single _ a)                 = Just a
indexJ i l | i >= sl                  = Nothing
  where sl = jlTagAsInt l
indexJ i (Append _ l1 l2) | i >= sl1  = indexJ (i - sl1) l2
                          | otherwise = indexJ i l1
  where sl1 = jlTagAsInt l1

dropJ :: (Sized b, Monoid b) => 
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i l | i <= 0 = l
dropJ i (Single _ _) = Empty
dropJ i l | i >= sl = Empty
  where sl = jlTagAsInt l
dropJ i (Append m l1 l2) | i < sl1   = (dropJ i l1) +++ l2
                         | otherwise = dropJ (i-sl1) l2
  where sl1 = jlTagAsInt l1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _ | i <= 0                   = Empty
takeJ _ Empty                        = Empty
takeJ _ s@(Single _ _)               = s
takeJ i l | i >= sl                  = l
  where sl = jlTagAsInt l
takeJ i (Append m l1 l2) | i < sl1   = takeJ i l1
                         | otherwise = l1 +++ (takeJ (i-sl1) l2)
  where sl1 = jlTagAsInt l1

-----------------Exercise 3-----------------
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
-----------------Exercise 4-----------------
fromList :: [String] -> JoinList (Score, Size) String
fromList []     = Empty
fromList (s:[]) = Single (scoreString s, 1) s
fromList ss = leftJL +++ rightJL
  where (firstHalf, secondHalf) = splitAt ((length ss + 1) `div` 2) ss
        leftJL = fromList firstHalf
        rightJL = fromList secondHalf

instance Buffer (JoinList (Score, Size) String) where

  -- | Convert a buffer to a String.
  toString = concat . jlToList

  -- | Create a buffer from a String.
  fromString s = fromList . lines $ s

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine _ _ Empty = Empty
  replaceLine i _ l | i < 0 = l
  replaceLine 0 s (Single _ _) = fromString s
  replaceLine i _ l | i >= sl = l
    where sl = jlTagAsInt l
  replaceLine i s (Append m l1 l2) | i < sl1   = (replaceLine i s l1) +++ l2
                                   | otherwise = l1 +++ (replaceLine (i-sl1) s l2)
    where sl1 = jlTagAsInt l1

  -- | Compute the number of lines in the buffer.
  numLines = jlTagAsInt

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value = getScore . fst . tag

buffer :: JoinList (Score, Size) String
buffer = fromString . unlines $ 
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor buffer
