import GHC.Exts (sortWith)

data CodeTree = Leaf Char Int | Fork CodeTree CodeTree [Char] Int
              deriving (Show)

weight (Leaf _ w)     = w
weight (Fork _ _ _ w) = w

chars (Leaf c _)      = [c]
chars (Fork _ _ cs _) = cs

makeCodeTree l r = Fork l r (chars l ++ chars r) (weight l + weight r)

times []       = []
times (x:xs) = (x, length (filter (== x) xs) + 1) : times (filter (/= x) xs)

makeOrderedLeafList f = map (uncurry Leaf) $ sortWith snd f

singleton [_] = True
singleton _   = False

combine (x:y:xs) = rec xs
  where c          = makeCodeTree x y
        rec []     = [c]
        rec (t:ts) = if weight t < weight c then t : rec ts else c : t : ts
combine xs       = xs

createCodeTree = head . (until singleton combine) . makeOrderedLeafList . times

type Bit = Int

decode tree bits = rec tree bits
  where rec t              []     = chars t
        rec (Leaf c _)     bs     = c : rec tree bs
        rec (Fork l r _ _) (b:bs) = if b == 0 then rec l bs else rec r bs

frenchCode = Fork (Fork (Fork (Leaf 's' 121895) (Fork (Leaf 'd' 56269) (Fork (Fork (Fork (Leaf 'x' 5928) (Leaf 'j' 8351) "xj" 14279) (Leaf 'f' 16351) "xjf" 30630) (Fork (Fork (Fork (Fork (Leaf 'z' 2093) (Fork (Leaf 'k' 745) (Leaf 'w' 1747) "kw" 2492) "zkw" 4585) (Leaf 'y' 4725) "zkwy" 9310) (Leaf 'h' 11298) "zkwyh" 20608) (Leaf 'q' 20889) "zkwyhq" 41497) "xjfzkwyhq" 72127) "dxjfzkwyhq" 128396) "sdxjfzkwyhq" 250291) (Fork (Fork (Leaf 'o' 82762) (Leaf 'l' 83668) "ol" 166430) (Fork (Fork (Leaf 'm' 45521) (Leaf 'p' 46335) "mp" 91856) (Leaf 'u' 96785) "mpu" 188641) "olmpu" 355071) "sdxjfzkwyhqolmpu" 605362) (Fork (Fork (Fork (Leaf 'r' 100500) (Fork (Leaf 'c' 50003) (Fork (Leaf 'v' 24975) (Fork (Leaf 'g' 13288) (Leaf 'b' 13822) "gb" 27110) "vgb" 52085) "cvgb" 102088) "rcvgb" 202588) (Fork (Leaf 'n' 108812) (Leaf 't' 111103) "nt" 219915) "rcvgbnt" 422503) (Fork (Leaf 'e' 225947) (Fork (Leaf 'i' 115465) (Leaf 'a' 117110) "ia" 232575) "eia" 458522) "rcvgbnteia" 881025) "sdxjfzkwyhqolmpurcvgbnteia" 1486387

secret = [0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1]

decodedSecret = decode frenchCode secret

encode tree text = foldr (++) [] $ map (rec tree) text
  where rec (Leaf _ _)     _ = []
        rec (Fork l r _ _) c = if elem c (chars l) then 0 : rec l c else 1 : rec r c

type CodeTable = [(Char, [Bit])]

codeBits (x:xs) c = if fst x == c then snd x else codeBits xs c

mergeCodeTables a b = map (prefix 0) a ++ map (prefix 1) b
  where prefix p (x, y) = (x, p : y)

convert (Leaf c _)     = [(c, [])]
convert (Fork l r _ _) = mergeCodeTables (convert l) (convert r)

quickEncode tree text = foldr (++) [] $ map (codeBits table) text
  where table = convert tree

{-- A faster implementation for `times':
times []       = []
times l@(x:xs) = (x, n) : times wo
  where rec []     = (0, [])
        rec (y:ys) = let (k, rest) = rec ys
                     in if x == y then (k + 1, rest) else (k, y : rest)
        (n, wo)    = rec l
--}

{-- `until' is part of the standard Prelude:
until :: (a -> Bool) -> (a -> a) -> a -> a  
until p f x | p x       =  x  
            | otherwise =  until p f (f x)
--}
