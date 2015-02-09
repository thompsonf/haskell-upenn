{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Living with some non-exhaustive pattern matching warnings
-- Would be nicer to update so that valid messages are one type,
-- invalid messages are another type

parseMessage :: String -> LogMessage
parseMessage s = parseMessageType (words s)

parseMessageType :: [String] -> LogMessage
parseMessageType ("I":xs)   = parseGivenMT xs Info
parseMessageType ("W":xs)   = parseGivenMT xs Warning
parseMessageType ("E":x:xs) = case reads x :: [(Int, String)] of
  [(n, "")] -> parseGivenMT xs (Error n)
  _         -> Unknown (unwords (x:xs))
parseMessageType xs = Unknown (unwords xs)

-- DOES NOT HANDLE CASE WHERE TIMESTAMP IS INVALID
parseGivenMT :: [String] -> MessageType -> LogMessage
parseGivenMT (x:xs) mt = case reads x :: [(TimeStamp, String)] of
  [(n, "")] -> LogMessage mt n (unwords xs)
  _         -> Unknown (unwords (x:xs))

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- pattern matching isn't exhaustive, but that's ok
-- we're assuming that every log message already in the tree
-- has a timestamp
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt          = mt
insert lm (Leaf)               = Node Leaf lm Leaf
insert lm@(LogMessage _ t _) (Node left nlm@(LogMessage _ nt _) right) = 
  if t < nt
  then Node (insert lm left) nlm right
  else Node left nlm (insert lm right)

build :: [LogMessage] -> MessageTree
build xs = foldr insert Leaf xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error n) _ _) = n >= 50
isSevere _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = [getMessage x | x <- inOrder (build xs), isSevere x]