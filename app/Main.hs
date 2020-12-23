{-# LANGUAGE GADTs, FlexibleContexts #-} 

module Main where
import Math.Geometry.Grid.Hexagonal ( paraHexGrid )
import qualified Math.Geometry.Grid as G
import qualified Math.Geometry.GridMap as M
import Math.Geometry.GridMap.Lazy ( lazyGridMap )
import Text.Read
import Data.List ( (\\), intersect, maximumBy )
import qualified Data.Map.Strict as Map
import Data.Typeable
import Control.Parallel.Strategies

computeValidMoves gm = M.filter (\v -> v /= 'A' && v /= 'B' ) gm

-- these guys return tuples
maxPA gm board max_depth curr_depth heur_func = do
        if curr_depth >= max_depth then
            ( heur_func gm board 'A', heur_func gm board 'B')
        else
            do
                let valid_moves = computeValidMoves gm
            
                -- valid_boards is of type [GridMap]
                let valid_boards = map (\k -> M.insert k 'A' gm) (M.keys valid_moves) `using` parList rseq


                -- board_values is of "type" [(GridMap, (Heuristic for A, Heuristic for B) )]
                let board_values = map ( \grid-> ( grid, ( maxPB (grid) (board) ( max_depth ) ( curr_depth + 1 ) ( heur_func ) ) ) ) valid_boards `using` parList rseq
                -- computes max, since comparisons are required here rseq is enough since it will need to evaluate to normal form here
                let board_max_value_A = maximumBy ( \(_,(heur_a_1,_)) (_,(heur_a_2,_)) -> compare heur_a_1 heur_a_2) board_values 

                
                (snd board_max_value_A)




maxPB gm board max_depth curr_depth heur_func = do
        if curr_depth >= max_depth then
            (heur_func gm board 'A', heur_func gm board 'B')
        else
            do
                let valid_moves = computeValidMoves gm
            
                -- valid_boards is of type [GridMap]
                let valid_boards = map (\k -> M.insert k 'B' gm) (M.keys valid_moves)`using` parList rseq

                -- board_values is of "type" [(GridMap, (Heuristic for A, Heuristic for B) )]
                let board_values = map ( \grid-> ( grid, ( maxPA  (grid) (board) ( max_depth ) ( curr_depth + 1 ) ( heur_func ) ) ) ) valid_boards `using` parList rseq

                let board_max_value_B = maximumBy ( \(_, (_,heur_b_1) ) (_, (_,heur_b_2 ) ) -> compare heur_b_1 heur_b_2  ) board_values 
                (snd board_max_value_B)


minimax_decision gm board color heuristic max_depth = do

    let valid_moves = computeValidMoves gm

    if color == 'A' then
        do
            let valid_boards = map (\k -> M.insert k 'A' gm) (M.keys valid_moves) `using` parList rseq


            let board_values = map ( \grid-> ( grid, ( maxPB  (grid) (board) ( max_depth ) ( 1 ) ( heuristic ) ) ) ) valid_boards  `using` parList rseq

            let board_max_value_A = maximumBy ( \(_,(heur_a_1,_)) (_,(heur_a_2,_)) -> compare heur_a_1 heur_a_2) board_values

            board_max_value_A
            
    else
        do
            let valid_boards = map (\k -> M.insert k 'B' gm) (M.keys valid_moves)  `using` parList rseq
 

            let board_values = map ( \grid-> ( grid, ( maxPA  (grid) (board) ( max_depth ) ( 1 ) ( heuristic ) ) ) ) valid_boards  `using` parList rseq

            let board_max_value_B = maximumBy ( \(_, (_,heur_b_1) ) (_, (_,heur_b_2 ) ) -> compare heur_b_1 heur_b_2  ) board_values 

            board_max_value_B
            
                   



playGame b_size gm board color heur_fn go_fn max_depth = do
    let game_over = go_fn gm board heur_fn

    case game_over of 
        0 ->  if color == 'A' then
                do 
                    let decision_gm_val = minimax_decision gm board color heur_fn max_depth
                    let decision_gm = fst decision_gm_val
                    putStrLn $ draw b_size decision_gm

                    playGame b_size decision_gm board 'B' heur_fn go_fn max_depth
              else
                do
                    let decision_gm_val = minimax_decision gm board color heur_fn max_depth
                    let decision_gm = fst decision_gm_val
                    putStrLn $ draw b_size decision_gm

                    playGame b_size decision_gm board 'A' heur_fn go_fn max_depth
        1 -> putStrLn "A wins"
        2 -> putStrLn "B wins"



basicGameOver gm board heur_fn = case (M.toList (computeValidMoves gm)) of
                            [] -> if heur_fn gm board 'A' >= heur_fn gm board 'B' then 1 else 2
                            _  -> 0    
        



-- get keys with value v from gm
-- getKeys gm v = filter (==v) gm

-- countConnected: find the number of pieces that are touching another piece of the same color on the board https://www.cs.swarthmore.edu/~bryce/cs63/s16/labs/hex.html
-- for all pieces, +1 if they are touching another piece
-- a piece is touching another piece if a coordinate belonging to a color is a neighbour of that piece
countConnected gm board color = sum $ map (\x -> countNeighbor x) coordinates
        where getCommonColors k v = v == color -- get kv pairs in grid map that have the same value as color
              coordinates = M.keys $ M.filterWithKey getCommonColors gm -- get list of coordinates belonging to that color (get keys with value v from gm)
              noNeighborIsSameColor = null . flip intersect coordinates . G.neighbours board -- returns boolean on whether a neighbour of the current point is a dot of the same color
              countNeighbor me = if noNeighborIsSameColor me then 0 else 1


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
      putStrLn (show (countConnected hex_b hex_grid 'B'))
      playGame 11 hex_b hex_grid 'A' (countConnected) (basicGameOver) 2



      






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
hex_b = lazyGridMap hex_grid 
    [
        'A', 'A', 'A', '-' , '-', '-', 'B', '-', 'B', 'B', 'A',
        'A', 'A', 'A', '-' , '-', '-', 'B', '-', 'B', 'B', 'A',
        'A', 'A', 'A', '-' , '-', '-', 'B', '-', 'B', 'B', 'A',
        'A', 'A', 'A', '-' , '-', 'B', 'B', '-', 'B', 'B', 'A',
        '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-',
        '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-',
        '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-',
        '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 
        '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 
        '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', 
        '-', '-', '-', '-', 'A', '-', '-', '-', '-', '-', '-'
    ]






























-- minimax_find_max f []     curr_max = curr_max
-- minimax_find_max f (m:ms) curr_max = if (f (fst m)) > (f (fst curr_max)) then minimax_find_max f ms m else minimax_find_max f ms curr_max 
-- 
-- gridmap, heuristic, depth_limit
--minimax gm heur_fn d_lim = p1Max gm heur_fn d_lim 0  --current depth is 0

-- minimumsFst :: Ord a => [(a, b)] -> [(a, b)]
-- minimumsFst [] = []
-- minimumsFst xs = filter ((==) minfst . fst) xs
--     where minfst = minimum (map fst xs)
-- 
-- -- heuristic = minimum remaining A hexes - minimum remaining B hexes and vice versa
-- -- inspiration: https://towardsdatascience.com/hex-creating-intelligent-adversaries-part-2-heuristics-dijkstras-algorithm-597e4dcacf93
-- -- candidates = list of candidate coordinates to perform heuristic function on (valid neighbors of existing point)
-- minAmongCandidates grid candidates = minimumsFst $ map (\point -> (point, minDistanceToBoundary grid point)) candidates
-- 
-- -- returns minimum distance between point and a boundary point e.g 1
-- -- not very smart cus does not take into consideration squares already taken lmao
-- minDistanceToBoundary grid point = foldr min 0 $ map (\boundaryPoint -> distance grid point boundaryPoint) (boundary grid) 
--allNeighborsExceptMe grid me everyone = map (neighbors grid) (filter (!= me) everyone)
-- remove all points that are not neighbors of the other (they must be reflexive to be next to each other) then find size of list where everybody is neighbor of each other
-- doesnt rly work if theres somebody out and about w component bigger than size 2, probably needs dfs 
-- longestConnected grid char gm = length $ filter (\x -> myNeighborIsADot grid x (filtered x)) pairs
--         where pairs = map fst (getKeys gm char)
--               filtered num = filter (\a -> num /= a) pairs

-- myNeighborIsADot grid me listOfFriends = not (Prelude.null (intersect (neighbours grid me) listOfFriends))
-- allPairsExceptMe me candidates = filter (/=me) candidates

-- find longest connected component, 
-- if a point has a neighbor
-- 	filter for the neighbors that are in the candidate list
-- 	do the same function for them 
-- 	if no more neighbors, return size
-- longestConnectedComponent grid point candidates size 
--    | myNeighborIsADot grid point candidates = f
--    | otherwise = size
--    where f = maximum $ map (\x -> longestConnectedComponent grid x candidates (size + 1)) (neighbours grid point)
-- 
-- h grid gm v = maximum $ map (\x -> longestConnectedComponent grid x pairs 0) pairs
--    where pairs = getKeys gm v





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
