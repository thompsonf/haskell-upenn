import Control.Applicative

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = fb

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (++) <$> (f <$> x) <*> (sequenceA xs)
  where f x = [x]

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA g xs = sequenceA $ g <$> xs

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa = sequenceA $ replicate n fa