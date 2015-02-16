{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Data.Function
import Data.Array

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

-----------------Exercise 2-----------------
-- get n dice sorted by value
dice :: Int -> Rand StdGen [DieValue]
dice 0 = return []
dice n =
  die        >>= \d ->
  dice (n-1) >>= \ds ->
  return (reverse . sort $ d:ds)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) = 
  diceA >>= \dA ->
  diceD >>= \dD ->
  let (aWins, bWins) = partition (==True) $ zipWith (>) dA dD
  in return $ Battlefield (att - length bWins) (def - length aWins)
  where diceA = dice $ min 3 (att-1)
        diceD = dice $ min 2 def
-----------------Exercise 3-----------------
invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield _ 0) = return b
invade b@(Battlefield n _) | n <= 1 = return b
invade b = battle b >>= \newB -> invade newB
-----------------Exercise 4-----------------
successProb :: Battlefield -> Rand StdGen Double
successProb b =
  numSuccessTrials 1000 b >>= \n ->
  return $ (fromIntegral n) / (fromIntegral 1000)

numSuccessTrials :: Int -> Battlefield -> Rand StdGen Int
numSuccessTrials 0 _ = return 0
numSuccessTrials n b =
  invade b >>= \newB ->
  numSuccessTrials (n-1) b >>= \succ ->
  return $ max 0 (1 - defenders newB) + succ
-----------------Exercise 5-----------------
-- With two attackers and 1 defender, success prob
-- is 5/12
-- if D rolls 6, A can't win
--    D rolls 5, A has 1 chance
--    D rolls 4, A has 2 chances
--    ...
--    D rolls 1, A has 5 chances
successA2D1 :: Double
successA2D1 = (fromIntegral 5) / (fromIntegral 12)

-- attacking with 2, defending with 1
-- if def rolls k prob that both att's rolls are <= k is
-- (k/6)^2
-- so overall prob that both att's rolls are <= def's roll
-- 1/6 * sum (k/6)^2 for k in [1..6]
-- prob of att winning is 125/216
successA3D1 :: Double
successA3D1 = (fromIntegral 125) / (fromIntegral 216)

failA3D1 :: Double
failA3D1 = 1 - successA3D1

-- attack with 3, defend with 1
-- if def rolls k, prob that all three att's rolls are <= k is
-- (k/6)^3
-- overall success prob
-- 1 - 1/6 * sum (k/6)^3 for k in [1..6] = 95/144
successA4D1 :: Double
successA4D1 = fromIntegral(95) / fromIntegral(144)

failA4D1 :: Double
failA4D1 = 1 - successA4D1


-- success only if attacker's roll is greater than both of defender's rolls
-- if att rolls k, prob that both def's rolls are < k is
-- (k-1)^2/36
-- total success prob is
-- 1/6 * sum (k-1)^2/36 for k in [1..6]
-- 55/216
successA2D2 :: Double
successA2D2 = (fromIntegral 55) / (fromIntegral 216)

failA2D2 :: Double
failA2D2 = 1 - successA2D2

-- just calculated these by writing a quick program to enumerate all possible dice rolls
-- and count how many of each situation happened
winbothA3D2 :: Double
winbothA3D2 = (fromIntegral 295) / (fromIntegral 1296)

splitA3D2 :: Double
splitA3D2 = (fromIntegral 420) / (fromIntegral 1296)

losebothA3D2 :: Double
losebothA3D2 = (fromIntegral 581) / (fromIntegral 1296)


winbothA4D2 :: Double
winbothA4D2 = (fromIntegral 2890) / (fromIntegral 7776)

splitA4D2 :: Double
splitA4D2 = (fromIntegral 2611) / (fromIntegral 7776)

losebothA4D2 :: Double
losebothA4D2 = (fromIntegral 2275) / (fromIntegral 7776)

exactSuccessProbArr :: Int -> Int -> Array (Int,Int) Double
exactSuccessProbArr a d = m
  where m :: Array (Int,Int) Double
        m = array ((1,0), (att, def)) $
              [((1,d), 0) | d <- [0..def]] ++
              [((a,0), 1) | a <- [2..att]] ++
              [((2,1), successA2D1)] ++
              [((3,1), successA3D1 + failA3D1 * successA2D1)] ++
              [((a,1), successA4D1 + failA4D1*m!(a-1,1)) | a <- [4..att]] ++
              [((2,d), successA2D2*m!(2,d-1)) | d <- [2..def]] ++
              [((3,d), winbothA3D2*m!(3,d-2) + splitA3D2*m!(2,d-1)) | d <- [2..def]] ++
              [((a,d), winbothA4D2*m!(a,d-2) + splitA4D2*m!(a-1,d-1) + losebothA4D2*m!(a-2,d)) | a <- [4..att], d <- [2..def]]
        att = max 3 a
        def = max 1 d

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield att def) = (exactSuccessProbArr att def)!(att,def)
