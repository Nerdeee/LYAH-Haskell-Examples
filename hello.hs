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