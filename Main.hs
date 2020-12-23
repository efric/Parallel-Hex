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


computeValidMoves gm = M.filter (\v -> v /= 'A' && v /= 'B' ) gm

maxPA gm board max_depth curr_depth heur_func = do
        if curr_depth >= max_depth then
            ( heur_func gm board 'A', heur_func gm board 'B')
        else
            do
                let valid_moves = computeValidMoves gm
            
                -- valid_boards is of type [GridMap]
                let valid_boards = map (\k -> M.insert k 'A' gm) (M.keys valid_moves)

                -- board_values is of "type" [(GridMap, (Heuristic for A, Heuristic for B) )]
                let board_values = map ( \grid-> ( grid, ( maxPB (grid) (board) ( max_depth ) ( curr_depth + 1 ) ( heur_func ) ) ) ) valid_boards
                
                let board_max_value_A = maximumBy ( \(_,(heur_a_1,_)) (_,(heur_a_2,_)) -> compare heur_a_1 heur_a_2) board_values
                
                (snd board_max_value_A)




maxPB gm board max_depth curr_depth heur_func = do
        if curr_depth >= max_depth then
            (heur_func gm board 'A', heur_func gm board 'B')
        else
            do
                let valid_moves = computeValidMoves gm
            
                -- valid_boards is of type [GridMap]
                let valid_boards = map (\k -> M.insert k 'B' gm) (M.keys valid_moves)
                -- board_values is of "type" [(GridMap, (Heuristic for A, Heuristic for B) )]
                let board_values = map ( \grid-> ( grid, ( maxPA  (grid) (board) ( max_depth ) ( curr_depth + 1 ) ( heur_func ) ) ) ) valid_boards

                let board_max_value_B = maximumBy ( \(_, (_,heur_b_1) ) (_, (_,heur_b_2 ) ) -> compare heur_b_1 heur_b_2  ) board_values
                (snd board_max_value_B)


minimax_decision gm board color heuristic max_depth = do

    let valid_moves = computeValidMoves gm

    if color == 'A' then
        do
            let valid_boards = map (\k -> M.insert k 'A' gm) (M.keys valid_moves)

            let board_values = map ( \grid-> ( grid, ( maxPB  (grid) (board) ( max_depth ) ( 1 ) ( heuristic ) ) ) ) valid_boards

            let board_max_value_A = maximumBy ( \(_,(heur_a_1,_)) (_,(heur_a_2,_)) -> compare heur_a_1 heur_a_2) board_values

            board_max_value_A
            
    else
        do
            let valid_boards = map (\k -> M.insert k 'B' gm) (M.keys valid_moves)

            let board_values = map ( \grid-> ( grid, ( maxPA  (grid) (board) ( max_depth ) ( 1 ) ( heuristic ) ) ) ) valid_boards

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
