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
main = putStrLn(show $ solve microban10)
--main = debugSolve ps

data Move = U | D | L | R deriving (Eq,Show)

data PuzzleBoard = Board {
                      walls :: [Pos],
                      goals :: [Pos]
                   } deriving (Eq,Show)

data PuzzleState = State PuzzleBoard Crates Worker deriving Show

instance Eq PuzzleState where
   State pb1 cs1 w1 == State pb2 cs2 w2 = (w1 == w2) && ((qsort cs1) == (qsort cs2))

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
--
-- What we're doing here is trying to reduce the search space by not
-- waiting until ALL the crates are stuck before deciding that there's
-- no point continuing
deadlocked :: PuzzleState -> Move -> Bool
deadlocked (State pb cs w) m = cInPos && (or [sic, tw, ft])
   where
      c = go w m
      cInPos = c `elem` cs
      sic = stuckInCorner pb c m
      tw = twoOnWall pb cs c m
      ft = fourTogether pb cs c m

-- Simplest deadlock check; have we been dumb and pushed a crate into
-- a corner that's not a goal
stuckInCorner :: PuzzleBoard -> Pos -> Move -> Bool
stuckInCorner pb c m = (not onGoal) && wallNext && (wallPerp1 || wallPerp2)
   where
      Board{walls = ws, goals = gs} = pb
      wallNext  = (go c m) `elem` ws
      wallPerp1 = (go c mp1) `elem` ws
      wallPerp2 = (go c mp2) `elem` ws
      onGoal    = c `elem` gs
      [mp1,mp2]   = perpendicular m

-- Another simple dealock case; have we got 2 crates together on a wall?
-- If they're not both on goals we're stuffed
twoOnWall :: PuzzleBoard -> Crates -> Pos -> Move -> Bool
twoOnWall pb cs c m = wallNext && ((cratePerp1 && wallNextCratePerp1 && (not (onGoalC && onGoalCP1))) ||
                                (cratePerp2 && wallNextCratePerp2 && (not (onGoalC && onGoalCP2))))
   where
      Board{walls = ws, goals = gs} = pb
      wallNext           = (go c m) `elem` ws
      cratePerp1         = (go c mp1) `elem` cs
      wallNextCratePerp1 = (go (go c mp1) m) `elem` ws
      cratePerp2         = (go c mp2) `elem` cs
      wallNextCratePerp2 = (go (go c mp2) m) `elem` ws
      onGoalC            = c `elem` gs
      onGoalCP1          = (go c mp1) `elem` gs
      onGoalCP2          = (go c mp2) `elem` gs
      [mp1,mp2]          = perpendicular m

-- TODO: Complete this deadlock case
--
-- Deadlock case 3; Four crates together in a square. There's
-- no escape unless they're all on goals.
fourTogether :: PuzzleBoard -> Crates -> Pos -> Move -> Bool
fourTogether pm cs c m = False

-- ######################################### --
-- ######################################### --
--                                           --
--  solve function and supporting functions  --
--                                           --
-- ######################################### --
-- ######################################### --

-- debugSolve just prints the number of states visited so far
-- every time we finish processing a "level" of the breadth first
-- search
debugSolve :: PuzzleState -> IO()
debugSolve p = debugSolve' ([(p,[])],[p])

debugSolve' :: ([(PuzzleState, Moves)],[PuzzleState]) -> IO()
debugSolve' (pms,pss) = do
   fs <- return(findSolutions pms)
   if fs == []
      then do putStrLn(show(length(pss)))
              gpms <- return(generate pms pss)
              if (gpms == (pms,pss))
                  then putStrLn("No Solutions, current states visited: " ++ show(pss))
                  else debugSolve' (gpms)
      else putStrLn(show(map reverse fs))

solve :: PuzzleState -> [Moves]
solve p = solve' ([(p,[])],[p])

solve' :: ([(PuzzleState, Moves)],[PuzzleState]) -> [Moves]
solve' (pms,pss) = case (findSolutions pms) of
   [] -> let gpms = generate pms pss
            in case (gpms == (pms,pss)) of
                  True -> error "No Solutions"
                  False -> solve' (generate pms pss)
   (ms:mss) -> map reverse (ms:mss)

findSolutions :: [(PuzzleState, Moves)] -> [Moves]
findSolutions [] = []
findSolutions ((p,m):pms) = case (isSolved p) of
   True  -> m:(findSolutions pms)
   False -> findSolutions pms

generate :: [(PuzzleState, Moves)] -> [PuzzleState] -> ([(PuzzleState, Moves)],[PuzzleState])
generate [] seen = ([],seen)
generate ((ps,ms):pms) seen = ((new_states ++ generated_states), new_seen_complete) 
   where
      new_states = [(newP, (m:ms)) | (newP,m) <- moves ps, not (newP `elem` seen), not (deadlocked newP m)]
      new_seen = addNewStates new_states seen
      (generated_states, new_seen_complete) = generate pms new_seen

addNewStates :: [(PuzzleState, Moves)] -> [PuzzleState] -> [PuzzleState]
addNewStates [] pss = pss
addNewStates ((ps,ms):pms) pss = addNewStates pms (ps:pss) 

isSolved :: PuzzleState -> Bool
isSolved (State pb cs w) = (qsort cs) == (qsort gs)
   where
      Board{goals = gs} = pb

moves :: PuzzleState -> [(PuzzleState, Move)]
moves ps = [ (ps', d) | d <- [U, D, L, R], ps' <- move ps d]

-- We're using lists a bit like a Maybe type here;
--  for Nothing, we return []
--  for Just x, we return [x]
move :: PuzzleState -> Move -> [PuzzleState]
move (State pb cs w) d
      | d1w = []
      | d1c = case d2_blocked of
                True  -> []
                False -> moveCrate (State pb cs d1) d1 (go d1 d)
      | otherwise = [(State pb cs d1)]
   where
      d1  = go w d 
      d1w = d1 `elem` ws
      d1c = d1 `elem` cs
      d2  = go d1 d
      d2_blocked = (d2 `elem` ws) || (d2 `elem` cs)
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
---- ########### ----
----             ----
----  Utilities  ----
----             ----
---- ########### ----
---- ########### ----

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x]
                ++ [x] ++
               qsort [y | y <- xs, y > x]

-- Simple utility function to find the moves perpendicular to the
-- direction we just went - used in deadlock checking.
perpendicular :: Move -> [Move]
perpendicular m = case (m `elem` [U,D]) of
   True -> [L,R]
   False -> [U,D]

-- ##################### --
-- ##################### --
--                       --
--  Puzzles for testing  --
--                       --
-- ##################### --
-- ##################### --

sasquatch1 :: PuzzleState
sasquatch1 = readPuzzleState ["   ###","  ## # ####"," ##  ###  #","## $      #","#   @$ #  #","### $###  #","  #  #..  #"," ## ##.# ##"," #      ##"," #     ##"," #######"]

microban1 :: PuzzleState
microban1 = readPuzzleState ["####","# .#","#  ###","#*@  #","#  $ #","#  ###","####"]

microban2 :: PuzzleState
microban2 = readPuzzleState ["######","#    #","# #@ #","# $* #","# .* #","#    #","######"]

microban3 :: PuzzleState
microban3 = readPuzzleState ["####","###  ####","#     $ #","# #  #$ #","# . .#@ #","#########"]

microban4 :: PuzzleState
microban4 = readPuzzleState ["########","#      #","# .**$@#","#      #","#####  #","    ####"]

microban5 :: PuzzleState
microban5 = readPuzzleState [" #######"," #     #"," # .$. #","## $@$ #","#  .$. #","#      #","########"]

microban6 :: PuzzleState
microban6 = readPuzzleState ["###### #####","#    ###   #","# $$     #@#","# $ #...   #","#   ########","#####"]

microban7 :: PuzzleState
microban7 = readPuzzleState ["#######","#     #","# .$. #","# $.$ #","# .$. #","# $.$ #","#  @  #","#######"]

microban8 :: PuzzleState
microban8 = readPuzzleState ["  ######","  # ..@#","  # $$ #","  ## ###","   # #","   # #","#### #","#    ##","# #   #","#   # #","###   #","  #####"]

microban9 :: PuzzleState
microban9 = readPuzzleState ["#####","#.  ##","#@$$ #","##   #"," ##  #","  ##.#","   ###"]

microban10 :: PuzzleState
microban10 = readPuzzleState ["      #####","      #.  #","      #.# #","#######.# #","# @ $ $ $ #","# # # # ###","#       #","#########"]

microban11 :: PuzzleState
microban11 = readPuzzleState ["  ######","  #    #","  # ##@##","### # $ #","# ..# $ #","#       #","#  ######","####"]

microban12 :: PuzzleState
microban12 = readPuzzleState ["#####","#   ##","# $  #","## $ ####"," ###@.  #","  #  .# #","  #     #","  #######"]

microban13 :: PuzzleState
microban13 = readPuzzleState ["####","#. ##","#.@ #","#. $#","##$ ###"," # $  #"," #    #"," #  ###"," ####"]

microban14 :: PuzzleState
microban14 = readPuzzleState ["#######","#     #","# # # #","#. $*@#","#   ###","#####"]

microban15 :: PuzzleState
microban15 = readPuzzleState ["     ###","######@##","#    .* #","#   #   #","#####$# #","    #   #","    #####"]

microban16 :: PuzzleState
microban16 = readPuzzleState [" ####"," #  ####"," #     ##","## ##   #","#. .# @$##","#   # $$ #","#  .#    #","##########"]

microban17 :: PuzzleState
microban17 = readPuzzleState ["#####","# @ #","#...#","#$$$##","#    #","#    #","######"]

microban18 :: PuzzleState
microban18 = readPuzzleState ["#######","#     #","#. .  #","# ## ##","#  $ #","###$ #","  #@ #","  #  #","  ####"]

microban19 :: PuzzleState
microban19 = readPuzzleState ["########","#   .. #","#  @$$ #","##### ##","   #  #","   #  #","   #  #","   ####"]

microban20 :: PuzzleState
microban20 = readPuzzleState ["#######","#     ###","#  @$$..#","#### ## #","  #     #","  #  ####","  #  #","  ####"]

