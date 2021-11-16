-- |

module Ch12Exercises where

-- Determine the kinds
-- 1. The kind is *
-- 2. The kind of a is * and the kind of f is * -> *

-- String processing
-- 1.
import Data.Either (partitionEithers)
notThe :: String -> Maybe String
notThe word =
  if word == "the"
  then Nothing
  else Just word

replaceThe :: String -> String
replaceThe = unwords . map replace . words
  where
    replace w = case notThe w of
      Nothing -> "a"
      Just word -> word

-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
  where
    count (w0:rest@(w1:ws)) =
      if isNothing (notThe w0) && head w1 `elem` "aeiou" -- see isNothing definition in exercise below
      then (+1) $ count rest
      else count rest
    count _ = 0

-- 3.
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = flip elem vowels

getVowels :: String -> String
getVowels = filter isVowel

countVowels :: String -> Integer
countVowels = toInteger . length . getVowels

-- Validate the word
newtype Word' =
  Word' String
  deriving (Eq, Show)

isWord :: String -> Bool
isWord candidate = fst t <= snd t
  where
    t = foldr (\a (numVowels, numConsonants)
               -> if isVowel a
                  then (numVowels+1, numConsonants)
                  else (numVowels, numConsonants+1))
        (0, 0) candidate

mkWord :: String -> Maybe Word'
mkWord candidate = if isWord candidate
                   then Just (Word' candidate)
                   else Nothing

-- It's only Natural
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | i == 0 = Just Zero
  | otherwise = Just $ go i
  where
    go 0 = Zero
    go i = Succ (go $ i - 1)

-- Small library for Maybe
-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe b = mayybee b id

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- 5.
catMaybes :: [Maybe a] -> [a]
--catMaybes = map (\(Just a) -> a) . filter isJust
catMaybes = foldr f []
  where
    f Nothing b = b
    f (Just a) b = a : b

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms =
  if any isNothing ms
  then Nothing
  else Just (map (\(Just a) -> a) ms)

-- Small library for Either
-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) acc = a : acc
    f (Right _) acc = acc

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Left _) acc = acc
    f (Right b) acc = b : acc

-- 3.
partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' = foldr f ([],[])
  where
    f (Left a) (as, bs) = (a:as, bs)
    f (Right b) (as, bs) = (as, b:bs)

-- 4.
eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b)= Just $ f b

-- 5.
either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

-- 6.
eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Unfold
-- 1.
myIterate :: (a  -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2.
myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f b
  | isNothing $ f b = []
  | otherwise = (fst $ t $ f b) : myUnfoldr f (snd $ t $ f b)
    where t (Just x) = x

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

-- Binary Trees
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold f a
  | isNothing $ f a = Leaf
  | otherwise = Node (unfold f x) y (unfold f z)
  where
    t = (\(Just t) -> t) $ f a
    x = (\(x, _, _) -> x) t
    y = (\(_, y, _) -> y) t
    z = (\(_, _, z) -> z) t

-- 2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f x = if x == n then Nothing else Just (x+1, x, x+1)
