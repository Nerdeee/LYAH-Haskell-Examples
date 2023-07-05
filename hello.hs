import qualified Data.Set as Set

main = do
  print "my first haskell program"
  name <- getLine
  print ("Hello, " ++ name)
f x y = x + y

doubleMe x = x * 2
doubleUs x y = doubleMe x + doubleMe y

list = [1,2,3]

yes = list ++ [4,5,6]

boomBangs xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]

length' xs = sum[1 | _ <- xs]

removeNonUpper st = [ c | c <- st, c `elem` ['A'..'Z']]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double  
circumference' r = 2 * pi * r  

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Out of luck"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first(x,_,_) = x

second :: (a,b,c) -> b
second(_, y, _) = y

head' :: [a] -> a
head' [] = error "cant call tail on empty list"
head' (x:_) = x

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height 
  | bmi <= 18.5 = "You're underweight"
  | bmi <= 25.0 = "You're supposedly normal"
  | bmi >= 30.0 = "You're overweight"
  | otherwise   = "You're a whale!"
    where bmi = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ

initials :: String -> String -> String
initials firstname lastname = "first initial: " ++ [f] ++ "second initial" ++ [l]
  where (f:_) = firstname
        (l:_) = lastname

calcBMIs :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea

front' :: [a] -> a
front' xs = case xs of [] -> error "no front, empty list"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "Empty"
                                               [x] -> "Singleton list"
                                               [xs] -> "A longer list"

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0 

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3+1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

sumByFolding :: (Num a) => [a] -> a
sumByFolding xs = foldl(\a b -> a + b) 0 xs

--rightFoldMap :: (a -> b) -> [a] -> [b]
--rightFoldMap f xs = (\x acc -> f x: acc ) [] xs

type PhoneNumber = String
type Name = String
phonebook :: [(Name, PhoneNumber)]
phonebook = [("jim","123 456 7890"),
             ("bob", "444 111 3450"),
             ("dylan", "559 867 5309")]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
  then Just v
  else findKey key xs

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)

data Person = Person String String Int deriving (Show, Eq, Read)

firstName :: Person -> String
firstName (Person firstname _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _) = lastname

age :: Person -> Int
age (Person _ _ age) = age

data Car = Car {
  company :: String,
  model :: String,
  year :: Int
} deriving (Show)

data Boat a b c = Boat {
  boatCompany :: a,
  boatModel :: b,
  boatYear :: c 
} deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

type AssocList k v = [(k,v)]

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
 | x == a = True
 | x < a = treeElem x left
 | x > a = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Tofu t where
  tofu :: j a -> t a j
