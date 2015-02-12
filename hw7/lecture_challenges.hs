import Data.Monoid (Monoid, mempty, mappend, mconcat)
-----------------Challenge 1-----------------
newtype AndBool = And Bool

newtype OrBool = Or Bool

instance Monoid (AndBool) where
  mempty = And True
  mappend (And x) (And y) = And $ x && y

instance Monoid (OrBool) where
  mempty = Or False
  mappend (Or x) (Or y) = Or $ x || y
-----------------Challenge 2-----------------
newtype Func a = Func (a -> a)

instance Monoid (Func a) where
  mempty = Func id
  mappend (Func f) (Func g) = Func $ f . g
