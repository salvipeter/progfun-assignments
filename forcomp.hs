import Control.Arrow ((&&&))
import Control.Monad.Reader
import Data.Char (toLower)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

type Word        = String
type Sentence    = [Word]
type Occurrences = [(Char, Int)]
type Dictionary  = Map Occurrences [Word]

-- same functionality as Scala's groupBy - can we do this faster?
groupIntoMapBy f = Map.fromList . map (f . head &&& id)
                   . groupBy ((==) `on` f) . sortBy (compare `on` f)

wordOccurrences         = map (head &&& length) . group . sort . map toLower
sentenceOccurrences     = wordOccurrences . foldr (++) ""
dictionaryByOccurrences = groupIntoMapBy wordOccurrences
anagramsByOccurrences :: Occurrences -> Reader Dictionary [Word]
anagramsByOccurrences x = asks (Map.lookup x) >>= return . (maybe [] id)
wordAnagrams            = anagramsByOccurrences . wordOccurrences

combinations []     = [[]]
combinations (x:xs) = [prefixIfNotZero i rest | i <- [0..snd x], rest <- combinations xs]
  where prefixIfNotZero i rest = if i == 0 then rest else (fst x, i) : rest

subtractOccurrences x y = convertBack $ Map.differenceWith diff (Map.fromList x) (Map.fromList y)
  where diff i j    = if i == j then Nothing else Just (i - j)
        convertBack = sortBy (compare `on` fst) . Map.toList

sentenceAnagrams :: Sentence -> Reader Dictionary [Sentence]
sentenceAnagrams = rec . sentenceOccurrences
  where rec     []     = return [[]]
        rec     xs     = fmap concat $ sequence [process xs occ | occ <- combinations xs]
        process xs occ = do rest  <- rec $ subtractOccurrences xs occ
                            words <- anagramsByOccurrences occ
                            return [w : r | w <- words, r <- rest]

-- Test

dictFile = "linuxwords.txt"

main = do dic <- fmap (dictionaryByOccurrences . lines) $ readFile dictFile
          mapM_ (putStrLn . show) $ runReader (sentenceAnagrams ["yes", "man"]) dic
