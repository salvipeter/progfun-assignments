import Control.Arrow ((&&&))
import Control.Monad.Reader
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Left, Right)
  
data Pos     = Pos   Int Int deriving (Eq, Ord, Show)
data Block   = Block Pos Pos deriving (Eq, Ord, Show)
type Terrain = Pos -> Bool
data Move    = Left | Right | Up | Down deriving (Show)
data Level   = Level { terrain :: Terrain, startPos :: Pos, goal :: Pos }

posDx (Pos x y) d = Pos (x + d) y
posDy (Pos x y) d = Pos x (y + d)

blockDx (Block p q) (d1, d2) = Block (posDx p d1) (posDx q d2)
blockDy (Block p q) (d1, d2) = Block (posDy p d1) (posDy q d2)

-- 1a
terrainFunction :: Array Int (Array Int Char) -> Pos -> Bool
terrainFunction t (Pos x _) | not $ inRange (bounds t) x       = False
terrainFunction t (Pos _ y) | not $ inRange (bounds (t ! 0)) y = False
terrainFunction t (Pos x y)                                    = t ! x ! y /= '-'
-- 1b
findChar :: Char -> Array Int (Array Int Char) -> Pos
findChar c t = let x = fst $ head $ filter (elem c . elems . snd) $ assocs t
                   y = fst $ head $ filter ((== c) . snd) $ assocs (t ! x)
               in Pos x y

-- 2a
isStanding :: Block -> Bool
isStanding (Block p q) = p == q
-- 2b
isLegal :: Block -> Reader Level Bool
isLegal (Block p q) = do t <- asks terrain
                         return $ t p && t q
-- 2c
startBlock :: Reader Level Block
startBlock = do sp <- asks startPos
                return $ Block sp sp

movement f s _ _ b                               | isStanding b = f b s
movement f _ x _ b@(Block (Pos px _) (Pos qx _)) | px == qx     = f b x
movement f _ _ y b                                              = f b y

left  = movement blockDy (-2, -1) (-1, -2) (-1, -1)
right = movement blockDy ( 1,  2) ( 2,  1) ( 1,  1)
up    = movement blockDx (-2, -1) (-1, -1) (-1, -2)
down  = movement blockDx ( 1,  2) ( 1,  1) ( 2,  1)

-- 3a
neighbors :: Block -> [(Block, Move)]
neighbors b = [(left b, Left), (right b, Right), (up b, Up), (down b, Down)]
-- 3b
legalNeighbors :: Block -> Reader Level [(Block, Move)]
legalNeighbors = filterM (isLegal . fst) . neighbors

-- 4a
done :: Block -> Reader Level Bool
done b@(Block p _) = do g <- asks goal
                        return $ isStanding b && p == g
-- 4b
neighborsWithHistory :: Block -> [Move] -> Reader Level [(Block, [Move])]
neighborsWithHistory b h = do n <- legalNeighbors b
                              return $ map (fst &&& (: h) . snd) n
-- 4c
newNeighborsOnly :: Set Block -> [(Block, [Move])] -> [(Block, [Move])]
newNeighborsOnly e = filter (flip Set.notMember e . fst)
-- 4d
from :: [(Block, [Move])] -> Set Block -> Reader Level [(Block, [Move])]
from [] _ = return []
from i  e = do n <- mapM (uncurry neighborsWithHistory) i
               let next_i = concat $ map (newNeighborsOnly e) n
               let next_e = Set.union (Set.fromList $ map fst i) e
               fmap (i ++) $ from next_i next_e
-- 4e/1
pathsFromStart :: Reader Level [(Block, [Move])]
pathsFromStart = do start <- startBlock
                    from [(start, [])] Set.empty
-- 4e/2
pathsToGoal :: Reader Level [(Block, [Move])]
pathsToGoal = do paths <- pathsFromStart
                 filterM (done . fst) paths
-- 4e/3
solution :: Reader Level [Move]
solution = do paths <- pathsToGoal
              return $ if null paths then [] else reverse $ snd (head paths)

-- Test

readLevel :: [String] -> Level
readLevel xs = Level (terrainFunction a) (findChar 'S' a) (findChar 'T' a)
  where n = length xs
        m = length (head xs)
        a = listArray (0, n-1) $ map (listArray (0, m-1)) xs

level0 = ["------",
          "--ST--",
          "--oo--",
          "--oo--",
          "------"]

level1 = ["ooo-------",
          "oSoooo----",
          "ooooooooo-",
          "-ooooooooo",
          "-----ooToo",
          "------ooo-"]

main = putStrLn $ show $ runReader solution (readLevel level1)

-- Check

step :: Move -> Block -> Block
step Left  = left
step Right = right
step Up    = up
step Down  = down

checkSolution :: Level -> [Move] -> Bool
checkSolution l xs = foldl (flip step) start xs == Block end end
  where start = runReader startBlock l
        end   = goal l

-- prop> checkSolution level $ runReader solution level
