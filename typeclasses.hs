-- type :info Eq and see class def. and etc.
eq :: Eq a => a -> a -> Bool
eq = (==)

neq :: Eq a => a -> a -> Bool
neq = (/=)

divideThenAdd :: (Num a, Fractional a) => a -> a -> a
divideThenAdd x y = (x / y) + 1

-- :type +d <value> to check default typeclass instance for value

data Mood = Blah | Blargh

instance Show Mood where
  show Blah = "h"
  show Blargh = "gh"

data Mood2 = Blah2 | Blargh2 deriving (Show)

-- silly typeclass. used to illustrate
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a -- bad!!

newtype Year
  = Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 42

newtype Month
  = Month Integer
  deriving (Eq, Show)

instance Numberish Month where
  fromNumber n = Month n
  toNumber (Month n) = n
  defaultNumber = Month 69

sumNumberish :: Numberish a => a -> a -> a
sumNumberish x y = fromNumber $ (+) (toNumber x) (toNumber y)

--

data Identity a
  = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity x) (Identity y) = x == y

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "dogs" "drool" "dogs"

