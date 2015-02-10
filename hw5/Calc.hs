{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import StackVM

import qualified Data.Map as M

-----------------Exercise 1-----------------
eval :: ExprT -> Integer
eval (Lit' x) = x
eval (Add' x y) = eval x + eval y
eval (Mul' x y) = eval x * eval y
-----------------Exercise 2-----------------
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit' Add' Mul' s of
  Just x  -> Just $ eval x
  Nothing -> Nothing
-----------------Exercise 3-----------------
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x   = Lit' x
  add x y = Add' x y
  mul x y = Mul' x y
-----------------Exercise 4-----------------
instance Expr Integer where
  lit x   = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x | x <= 0    = False
        | otherwise = True
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit x                     = MinMax x
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x                 = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
-- print testExp with types Maybe Integer, Maybe Bool, etc
-----------------Exercise 5-----------------
-- treat programs as integer manipulation
instance Expr Program where
  lit x     = [PushI x]
  add xs ys = xs ++ ys ++ [Add]
  mul xs ys = xs ++ ys ++ [Mul]

--treat programs as bool manipulation
{-instance Expr Program where
  lit x | x <= 0    = [PushB False]
        | otherwise = [PushB True]
  add xs ys = xs ++ ys ++ [Or]
  mul xs ys = xs ++ ys ++ [And]-}

compile :: String -> Maybe Program
compile s = parseExp lit add mul s

stackEval :: Maybe Program -> Either String StackVal
stackEval maybexs = maybe (Left "Error parsing input") stackVM maybexs
-----------------Exercise 6-----------------
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit x   = VLit x
  add x y = VAdd x y
  mul x y = VMul x y

instance HasVars VarExprT where
  var s = VVar s

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x     = const . Just $ x
  add f g m = case (f m, g m) of
                (Just x, Just y) -> Just $ x + y
                _                -> Nothing
  mul f g m = case (f m, g m) of 
                (Just x, Just y) -> Just $ x * y
                _                -> Nothing

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
