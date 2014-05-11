contains     s x = s x
singletonSet e x = x == e

union     s t x = s x || t x
intersect s t x = s x && t x
diff      s t x = s x && not (t x)
filter          = intersect

bound = 1000

forall s p = null [x | x <- [-bound..bound], s x && not (p x)]
exists s p = not $ forall s (\x -> not $ p x)

map s f y = not $ null [x | x <- [-bound..bound], f x == y && s x]

setToList s = [x | x <- [-bound..bound], s x]
printSet = putStrLn . show . setToList
