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
  -- end of syntax in functions chapter (should've added this comment for the previous chapter but I'm lazy)
