{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Monoid
import Employee
import Data.Tree
import Data.List (sort)
-----------------Exercise 1-----------------
glCons :: Employee -> GuestList -> GuestList
glCons e (GL gs f) = GL (e : gs) (empFun e + f)

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend (GL gs1 f1) (GL gs2 f2) = GL (gs1 ++ gs2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) | f1 >= f2  = gl1
                                    | otherwise = gl2
-----------------Exercise 2-----------------
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node {rootLabel = x, subForest = ts}) = f x $ map (treeFold f) ts
-----------------Exercise 3-----------------
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (GL [e] (empFun e), mempty)
nextLevel e xs = (withE, withoutE)
  where
  	-- For some reason putting this on the same line as the 'where'
  	-- was causing a parse error...
  	withoutE = mconcat $ map fst xs
  	withE = glCons e $ mconcat (map snd xs)
-----------------Exercise 4-----------------
maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold nextLevel t
-----------------Exercise 5-----------------
getFunStr :: Fun -> String
getFunStr fun = "Total fun: " ++ (show fun)

getOutStr :: Tree Employee -> String
getOutStr tree = (getFunStr fun) ++ "\n" ++ (unlines . sort $ map empName gs)
  where (GL gs fun) = maxFun tree

-- without do syntax
main = readFile "company.txt" >>= (putStrLn . getOutStr . read)

-- with do syntax
-- main :: IO ()
-- main = do str <- readFile "company.txt"
--           putStrLn . getOutStr . read $ str
