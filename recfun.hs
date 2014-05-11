-- Assignment 1 in Haskell

pascalNaive c r | c == 0 || c == r = 1
pascalNaive c r                    = left + right
  where left  = pascalNaive (c-1) (r-1)
        right = pascalNaive   c   (r-1)

-- David verziojanak parafrazisa
pascalDavid c r = last $ iterate col start !! c
  where start        = take (r-c+1) $ repeat 1
        col [x]      = [x]
        col (x:y:ys) = x : (col $ x+y : ys)

-- Gyors verzio
pascal c r = rec r 1 1
  where rec _ k acc | k > c = acc
        rec n k acc         = rec (n-1) (k+1) (acc * n `div` k)

pascalRowString row = concat $ map ((++ " ") . show) xs
  where xs = [pascal col row | col <- [0,1..row]]

main = do
  putStrLn "Pascal's Triangle"
  mapM_ (putStrLn . pascalRowString) [0,1..10]

balance chars = rec chars 0
  where rec []         open = open == 0
        rec ('(':tail) open = rec tail (open+1)
        rec (')':tail) open = open > 0 && rec tail (open-1)
        rec (_:tail)   open = rec tail open

countChange 0    _              = 1
countChange _    []             = 0
countChange m c@(x:xs) | x <= m = countChange (m-x) c + countChange m xs
countChange m   (_:xs)          = countChange m xs
