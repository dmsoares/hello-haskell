-- |

module CH11ADT where

-- sample data
jm = Person "julie" 108
ca = Person "chris" 16

-- Record syntax
data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

-- Normal form
-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show

-- data BookType = Fictionbook Fiction
              -- | NonfictionBook Nonfiction
              -- deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType)

data Author = Fiction AuthorName
            | Nonfiction AuthorName

-- Constructing and deconstructing values
data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)

-- -- Sum and Product
newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

f :: Product a (Product b NumSheep) -> Int -- this function will accept any Product value whose
f (Product _ (Product _ (NumSheep x))) = x -- second argument to its second parameter has type NumSheep

f' :: BigFarmhouse' -> Int                   -- this will only accept values of type Product whose construction
f' (Product _ (Product _ (NumSheep x))) = x  -- abides by the 'structure' imposed by BigFarmhouse'

-- -- same trick with Sum
type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

shaun = Second $ Second $ SheepInfo "Shaun" 3 1 :: Animal'
