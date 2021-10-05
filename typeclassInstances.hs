-- | Chapter 6

module TypeClassInstances where
import Data.List (sort)

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date =
  Date DayOfWeek Int deriving Show

instance Eq Date where
  (==) (Date weekday dayOfMonth)
        (Date weekday' dayOfMonth') =
    weekday == weekday'
    && dayOfMonth == dayOfMonth'

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- Exercises --
--1.
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b

--2.
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = a == c && b == d

--3.
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a == b
  (==) (TisAString a) (TisAString b) = a == b
  (==) _ _ = False

--4.
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair c d) = a == c && b == d

--5.
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple c d) = a == c && b == d

--6.
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

--7.
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False

--Ord instances

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ


-- Does it typecheck?
-- 1.

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.

data Mood = Blah | Woot deriving (Show, Eq, Ord)

settleDown x = if x == Woot
  then Blah
  else x

-- 4.

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?

data Rocks = Rocks String deriving (Eq, Ord, Show)

data Yeah = Yeah Bool deriving (Eq, Ord, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Ord, Show)

-- 1.
phew = Papu (Rocks "chases") (Yeah True)

-- 2.
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4.
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types

-- 1.
i :: Num a => a
i = 1

-- 2.
f :: Float
f = 1.0

-- 3.
f' :: Fractional a => a
f' = 1.0

-- 4.
g :: RealFrac a => a
g = 1.0

-- 5.
freud :: Ord a => a -> a
freud x = x

-- 6.
freud' :: Int -> Int
freud' x = x

-- 7.
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

--8.
myY = 1 :: Int

sigmund' :: Int -> Int
sigmund' y = myY

-- 9.
-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- 10.
-- jung' :: [Char] -> Char
jung' :: Ord a => [a] -> a
jung' xs = head (sort xs)

-- 11.
mySort :: [Char] ->  [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- Type-Kwon-Do Two
-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
-- chk x y z = z == x y
chk x y z = x y == z

-- 2.
arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith aToB y z = aToB z
