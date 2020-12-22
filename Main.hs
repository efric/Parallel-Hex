{-# LANGUAGE GADTs, FlexibleContexts #-} 

module Main where
import Math.Geometry.Grid.Hexagonal ( paraHexGrid )
import Math.Geometry.Grid
import qualified Math.Geometry.GridMap as M
import Math.Geometry.GridMap.Lazy ( lazyGridMap )
import Text.Read
import Data.List ( (\\), intersect )
import qualified Data.Map.Strict as Map

computeValidMoves :: (Eq (Index (M.BaseGrid gm b)), M.GridMap gm b) => gm b -> [Index (M.BaseGrid gm b)]
computeValidMoves grid_map = (get_positions grid_map) \\ (used_tiles grid_map)
    where get_positions grid_map = indices $ M.toGrid grid_map
          used_tiles grid_map = [fst x | x <- M.toList grid_map]

-- We should make a heuristic function here: a basic one is counting the number of connected as in here: https://www.cs.swarthmore.edu/~bryce/cs63/s16/labs/hex.html


-- getSameColor color grid_list = [fst x | x <- grid_list, snd x == color]
-- 
-- 
-- p1Max = pxMax 'A'
-- 
-- p2Max = pxMax 'B'
-- 
-- 
-- pxMax color grid_map heuristic_fn depth_limit curr_depth = 
--     if curr_depth >= depth_limit 
--     then ( (heuristic_fn 'A' grid_map, heuristic_fn 'B' grid_map), (-1, -1) ) --do the heuristic function
-- 	--heuristic function for calling A and B, returns a number (3 etc)
--     else 
--         case color of
--             'A' -> minimax_find_max (fst) (recPxMap1) (head recPxMap1) -- list of tuples
--             'B' -> minimax_find_max (snd) (recPxMap2) (head recPxMap2) 
--     where 
--             anti_color = case color of 
--                             'A' -> 'B'
--                             'B' -> 'A'
--             -- The next two functions return, basically, a list of ( (heuristic value for A, heuristic value for B), move), where move is of the type (a,b) in parallelogram notation
--             recPxMap1 = map (\move -> ( fst (p2Max (M.insert move color grid_map) heuristic_fn depth_limit (curr_depth + 1) ), move) )  (computeValidMoves grid_map) -- call min/max (whatever the opposite)
--             recPxMap2 = map (\move -> ( fst (p1Max (M.insert move color grid_map) heuristic_fn depth_limit (curr_depth + 1) ), move) )  (computeValidMoves grid_map) 
--                                                             
-- 
-- minimax_find_max f []     curr_max = curr_max
-- minimax_find_max f (m:ms) curr_max = if (f (fst m)) > (f (fst curr_max)) then minimax_find_max f ms m else minimax_find_max f ms curr_max 
-- 
-- gridmap, heuristic, depth_limit
--minimax gm heur_fn d_lim = p1Max gm heur_fn d_lim 0  --current depth is 0

minimumsFst :: Ord a => [(a, b)] -> [(a, b)]
minimumsFst [] = []
minimumsFst xs = filter ((==) minfst . fst) xs
    where minfst = minimum (map fst xs)

-- heuristic = minimum remaining A hexes - minimum remaining B hexes and vice versa
-- inspiration: https://towardsdatascience.com/hex-creating-intelligent-adversaries-part-2-heuristics-dijkstras-algorithm-597e4dcacf93
-- candidates = list of candidate coordinates to perform heuristic function on (valid neighbors of existing point)
minAmongCandidates grid candidates = minimumsFst $ map (\point -> (point, minDistanceToBoundary grid point)) candidates

-- returns minimum distance between point and a boundary point e.g 1
-- not very smart cus does not take into consideration squares already taken lmao
minDistanceToBoundary grid point = foldr min 0 $ map (\boundaryPoint -> distance grid point boundaryPoint) (boundary grid) 

-- get keys with value v from gm
getKeys gm v = filter (==v) gm

--allNeighborsExceptMe grid me everyone = map (neighbors grid) (filter (!= me) everyone)
-- remove all points that are not neighbors of the other (they must be reflexive to be next to each other) then find size of list where everybody is neighbor of each other
-- doesnt rly work if theres somebody out and about w component bigger than size 2, probably needs dfs 
longestConnected grid char gm = length $ filter (\x -> myNeighborIsADot grid x (filtered x)) pairs
        where pairs = map fst (getKeys gm char)
              filtered num = filter (\a -> num /= a) pairs

myNeighborIsADot grid me listOfFriends = not (Prelude.null (intersect (neighbours grid me) listOfFriends))

-- ask user for how much time AI should spend
askTime :: IO Double
askTime = do
    putStrLn "How many seconds should the AI have to compute each move? (Default is 0.2)"
    response <- getLine
    case response of
        "" -> return 0.2
        s -> case readMaybe s of --parse input
            Just n -> return n --if number, return that number
            Nothing -> askTime --if it aint, ask again

-- ask user for how big the board should be 
askSize :: IO Integer
askSize = do
    putStrLn "How big should the Hex board be? (Default is 11)"
    response <- getLine
    case response of
        "" -> return 11
        s -> case readMaybe s of --parse input and make sure that it's int
            Just n -> return n
            Nothing -> askSize

main :: IO ()
main = do 
--      time <- askTime
--      size <- askSize
--      putStrLn (draw 26 hex_b) -- hardcoded example
--      putStrLn (draw 26 (M.insert (3,3) 'a' hex_b))
      return ()

-- not sure how to do function signature w external library yet..
-- draw :: Int -> GridMap -> String basically
draw size board = unlines [line y | y <- [0..size]] where
    line 0 = (' ':) $ take size ['A'..] >>= (:" ") --draw top legend
    line y = replicate y ' ' ++ --white space for formatting
             replicate (length $ show y) '\b' ++  -- delete extra spaces
             show y ++ -- print current number
             concat [[' ', cell x (y - 1)] | x <- [0..size-1]] -- actual line info
    cell x y = case M.lookup (x, y) board of
                    Just a -> a -- change this w actual data type used by value (this one needs FlexibleContexts)
                    Nothing -> '-'

hex_grid = paraHexGrid 11 11
hex_b = lazyGridMap hex_grid ['a', 'b', 'c']
