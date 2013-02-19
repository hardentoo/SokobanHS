--module Sokoban (
--   PuzzleState(State),
--   PuzzleBoard(Board),
--   Pos,
--   Crate,
--   Crates,
--   Worker,
--   Move,
--   Moves,
--
--   solve,
--   readPuzzleState
--) where

module Main (
   main
) where

main :: IO()
main = putStrLn(show $ solve ps)

ps :: PuzzleState
ps = readPuzzleState ["####","# .#","#  ###","#*@  #","#  $ #","#  ###","####"]

data Move = U | D | L | R deriving (Eq,Show)

data PuzzleBoard = Board {
                      walls :: [Pos],
                      goals :: [Pos]
                   } deriving (Eq,Show)

data PuzzleState = State PuzzleBoard Crates Worker deriving (Eq,Show)

type Moves = [Move]
type Pos = (Int, Int)
type Crate = Pos
type Crates = [Crate]
type Worker = Pos

readPuzzleState :: [String] -> PuzzleState
readPuzzleState ss = readPuzzleState' emptyState (0,0) ss
   where
      readPuzzleState' st p [] = st
      readPuzzleState' st p@(x,y) (s:ss) = readPuzzleState' (readPuzzleState'' st p s) (x,y+1) ss

      readPuzzleState'' st p [] = st
      readPuzzleState'' st p@(x,y) (c:cs) = case c of
            '#' -> readPuzzleState'' (addWall p st) (x+1,y) cs
            '.' -> readPuzzleState'' (addGoal p st) (x+1,y) cs
            '$' -> readPuzzleState'' (addCrate p st) (x+1,y) cs
            '@' -> readPuzzleState'' (addWorker p st) (x+1,y) cs
            '*' -> readPuzzleState'' (addCrate p $ addGoal p st) (x+1,y) cs
            otherwise -> readPuzzleState'' st (x+1,y) cs

      addWall pos (State pb cs w) = let pb' = pb{walls = (pos:) . walls $ pb}
                                     in State pb' cs w 

      addGoal pos (State pb cs w) = let pb' = pb{goals = (pos:) . goals $ pb}
                                     in State pb' cs w 

      addCrate pos (State pb cs w) = State pb (pos:cs) w

      addWorker pos (State pb cs w) = State pb cs pos

      emptyState = State (Board{walls = [],goals = []}) [] (-1,-1)

-- Let's not check the whole puzzle state each time
-- instead let's try just checking the crates next to the worker.
-- These should find the deadlocks quicker in crates we just moved
-- and we'll find anything else eventually :)
deadlocked :: PuzzleState -> Move -> Bool
deadlocked (State pb cs w) m = cInPos && (or [sic, tw, ft])
   where
      c = go w m
      cInPos = c `elem` cs
      sic = stuckInCorner pb c m
      tw = twoOnWall pb c
      ft = fourTogether pb c

stuckInCorner :: PuzzleBoard -> Pos -> Move -> Bool
stuckInCorner pb c m = (not onGoal) && (wallNext && (wallAbove || wallBelow))
   where
      Board{walls = ws, goals = gs} = pb
      wallNext  = (go c m) `elem` ws
      wallAbove = (go c mr) `elem` ws
      wallBelow = (go c ml) `elem` ws
      onGoal    = c `elem` gs
      [mr,ml]   = perpendicular m

twoOnWall :: PuzzleBoard -> Pos -> Bool
twoOnWall pb c = False

fourTogether :: PuzzleBoard -> Pos -> Bool
fourTogether pm c = False

perpendicular :: Move -> [Move]
perpendicular m = case (m `elem` [U,D]) of
   True -> [L,R]
   False -> [U,D]
--   where
--      crateAbove
--      crateBelow
--      crateLeft
--      crateRight

--deadlockedCrate p c m
--   where
--      cratesNear (x,y) = [c | c@(x',y') <- cs, ((x==x') && (abs (x-x') < 2)) || ((abs (y-y') < 2) && (y==y'))]

-- A Crate is deadlocked if it's not on a goal and it is blocked
-- by a wall opposite any open space...
-- There are other more complex stuck states (e.g. blocked by another
-- deadlocked crate but I'm hoping I shouldn't hit those)
--deadlockedCrate :: PuzzleState -> Crate -> Move -> Bool
--deadlockedCrate (State pb cs w) c m = 
--   where 

solve :: PuzzleState -> [Moves]
solve p = solve' ([(p,[])],[p])

solve' :: ([(PuzzleState, Moves)],[PuzzleState]) -> [Moves]
solve' (pms,pss) = case (findSolutions pms) of
   [] -> solve' (generate pms pss)
   (ms:mss) -> map reverse (ms:mss)

findSolutions :: [(PuzzleState, Moves)] -> [Moves]
findSolutions [] = []
findSolutions ((p,m):pms) = case (isSolved p) of
   True  -> m:(findSolutions pms)
   False -> findSolutions pms

generate :: [(PuzzleState, Moves)] -> [PuzzleState] -> ([(PuzzleState, Moves)],[PuzzleState])
generate pms seen
   | newStates == [] = error "Puzzle Can't be solved"
   | otherwise       = (newStates, newSeen)
      where
         newStates = [(newP, (m:ms)) | (p,ms) <- pms,
                                       (newP, m) <- (moves p),
                                       not (deadlocked newP m),
                                       not (newP `elem` seen)]
         newSeen = addNewStates newStates seen

addNewStates :: [(PuzzleState, Moves)] -> [PuzzleState] -> [PuzzleState]
addNewStates [] pss = pss
addNewStates ((ps,ms):pms) pss = addNewStates pms (ps:pss) 

-- define isSolution and move, then we can start debugging :)
isSolved :: PuzzleState -> Bool
isSolved (State pb cs w) = (qsort cs) == (qsort ws)
   where
      Board{goals = ws} = pb

---- ######### ----
----  WORKING  ----
---- ######### ----

moves :: PuzzleState -> [(PuzzleState, Move)]
moves ps = [ (ps', d) | d <- [U, D, L, R], ps' <- move ps d]

-- We're using lists a bit like a Maybe type here;
--  for Nothing, we return []
--  for Just x, we return [x]
move :: PuzzleState -> Move -> [PuzzleState]
move (State pb cs w) d
      | d1w = []
      | d1c = moveCrate (State pb cs d1) d1 (go d1 d)
      | otherwise = [(State pb cs d1)]
   where
      d1  = go w d 
      d1w = d1 `elem` ws
      d1c = d1 `elem` cs
      Board{walls = ws} = pb

go :: Pos -> Move -> Pos
go (x,y) U = (x,y-1)
go (x,y) D = (x,y+1)
go (x,y) L = (x-1,y)
go (x,y) R = (x+1,y)

moveCrate :: PuzzleState -> Pos -> Pos -> [PuzzleState]
moveCrate (State pb cs w) p1 p2 = [State pb (replaceFirst p1 p2 cs) w]

replaceFirst :: Eq a => a -> a -> [a] -> [a]
replaceFirst a b [] = error "Crate not found!" 
replaceFirst a b (x:xs)
   | a == x = b:xs
   | otherwise = x:(replaceFirst a b xs)

---- ########### ----
----  Utilities  ----
---- ########### ----

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x]
                ++ [x] ++
               qsort [y | y <- xs, y > x]

