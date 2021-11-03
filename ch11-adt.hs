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

-- Constructing values
-- Nullary
trivialValue :: GuessWhat
trivialValue = Chickenbutt

-- Unary
idInt :: Id Integer
idInt = MkId 10

-- Product
type Awesome = Bool
-- type Name = String

person :: Product Name Awesome
person = Product "Simon" True

-- Sum
data Twitter =
  Twitter deriving (Eq, Show)

data AskFm =
  AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork =  First Twitter

-- Record
myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001  -- here, accessors can also be used to retrieve values

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct { pfirst = 42
                          , psecond = 0.00001 }

myRecord'' :: RecordProduct Integer Float
myRecord'' = RecordProduct { psecond = 0.00001 -- record syntax means we can reorder stuff
                           , pfirst = 42 }

-- Deconstructing values
newtype Name' = Name' String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

data Farmer =
  Farmer Name' Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- ...with record syntax
data FarmerRec =
  FarmRec { name' :: Name
          , acres :: Acres
          , farmerType :: FarmerType }
          deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer ->  True
    _           -> False

-- !! once again, do not propagate bottoms !!
-- don't do this:
-- data Automobile = Null
--                 | Automobile { make :: String
--                              , model :: String }
-- the above can lead to using accessors in places where
-- the Null value is being given. The compiler won't
-- complain, leading to a runtime error.
-- Below is a possible solution (but be advised that
-- the use of a Null value is not recommended: use Maybe instead).

data Car = Car { make :: String
               , model :: String }
               deriving (Eq, Show)

data Automobile = Null
                | Automobile Car
                deriving (Eq, Show)

getMake :: Automobile -> String
getMake (Automobile c) = make c
getMake _ = ""

make' = getMake Null

-- Function type as exponentiation
data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

convert :: Quantum -> Bool
convert Yes = True
convert _ = False

convert2 :: Quantum -> Bool
convert2 No = True
convert2 _ = False

convert3 :: Quantum -> Bool
convert3 Both = True
convert3 _ = False

convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 _ = True

convert5 :: Quantum -> Bool
convert5 No = False
convert5 _ = True

convert6 :: Quantum -> Bool
convert6 Both = False
convert6 _ = True

convert7 :: Quantum -> Bool
convert7 _ = True

convert8 :: Quantum -> Bool
convert8 _ = False

-- Binary Trees
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

insert' :: Ord a
           => a
           -> BinaryTree a
           -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node l v r)
  | b == v = Node l v r
  | b < v = Node (insert' b l) v r
  | b > v = Node l v (insert' b r)
