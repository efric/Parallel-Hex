{-# LANGUAGE GADTs, FlexibleContexts #-} 

module Main where
import Math.Geometry.Grid.Hexagonal ( paraHexGrid )
import qualified Math.Geometry.Grid as G
import qualified Math.Geometry.GridMap as M
import Math.Geometry.GridMap.Lazy ( lazyGridMap )
import Text.Read
import Data.List (intersect, maximumBy )
import Control.Parallel.Strategies

computeValidMoves :: M.GridMap gm Char => gm Char -> gm Char
computeValidMoves gm = M.filter (\v -> v /= 'A' && v /= 'B' ) gm

maxPA :: (M.GridMap gm Char, Num a, Ord a, Ord (G.Index (M.BaseGrid gm Char)), Ord b) => gm Char -> t -> a -> a -> (gm Char -> t -> Char -> b) -> (b, b)
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
                -- computes max, since comparisons are required here rseq is enough since it will need to evaluate to normal form here
                let board_max_value_A = maximumBy ( \(_,(heur_a_1,_)) (_,(heur_a_2,_)) -> compare heur_a_1 heur_a_2) board_values 

                
                (snd board_max_value_A)



maxPB :: (M.GridMap gm Char, Num a1, Ord a1, Ord (G.Index (M.BaseGrid gm Char)), Ord a2) => gm Char -> t -> a1 -> a1 -> (gm Char -> t -> Char -> a2) -> (a2, a2)
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

minimax_decision :: (M.GridMap gm Char, Num a1,
                           Ord (G.Index (M.BaseGrid gm Char)), Ord a1, Ord a2) =>
                          gm Char
                          -> t
                          -> Char
                          -> (gm Char -> t -> Char -> a2)
                          -> a1
                          -> (gm Char, (a2, a2))
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


par_minimax_decision :: (M.GridMap gm Char, Num a1,
                               Ord (G.Index (M.BaseGrid gm Char)), Ord a1, Ord a2) =>
                              gm Char
                              -> t
                              -> Char
                              -> (gm Char -> t -> Char -> a2)
                              -> a1
                              -> (gm Char, (a2, a2))
par_minimax_decision gm board color heuristic max_depth = do

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
            
par_playGame :: (M.GridMap gm Char, Num a1, Num a2,
                       Ord (G.Index (M.BaseGrid gm Char)), Ord a2, Ord a3, Eq a1,
                       G.Index (M.BaseGrid gm Char) ~ (Int, Int)) =>
                      Int
                      -> gm Char
                      -> t
                      -> Char
                      -> (gm Char -> t -> Char -> a3)
                      -> (gm Char -> t -> (gm Char -> t -> Char -> a3) -> a1)
                      -> a2
                      -> IO ()
par_playGame b_size gm board color heur_fn go_fn max_depth = do
    
    let game_over = go_fn gm board heur_fn

    case game_over of 
        0 ->  if color == 'A' then
                do 
                    let decision_gm_val = par_minimax_decision gm board color heur_fn max_depth
                    let decision_gm = fst decision_gm_val
                    putStrLn $ draw b_size decision_gm

                    playGame b_size decision_gm board 'B' heur_fn go_fn max_depth
              else
                do
                    let decision_gm_val = par_minimax_decision gm board color heur_fn max_depth
                    let decision_gm = fst decision_gm_val
                    putStrLn $ draw b_size decision_gm

                    playGame b_size decision_gm board 'A' heur_fn go_fn max_depth
        1 -> putStrLn "A wins"
        2 -> putStrLn "B wins"
        _ -> error "Error in game processing"


playGame :: (M.GridMap gm Char, Num a1, Num a2,
                   Ord (G.Index (M.BaseGrid gm Char)), Ord a2, Ord a3, Eq a1,
                   G.Index (M.BaseGrid gm Char) ~ (Int, Int)) =>
                  Int
                  -> gm Char
                  -> t
                  -> Char
                  -> (gm Char -> t -> Char -> a3)
                  -> (gm Char -> t -> (gm Char -> t -> Char -> a3) -> a1)
                  -> a2
                  -> IO ()
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
        _ -> error "Error in game processing"


basicGameOver :: (M.GridMap gm Char, Ord a, Num p) => gm Char -> t -> (gm Char -> t -> Char -> a) -> p
basicGameOver gm board heur_fn = case (M.toList (computeValidMoves gm)) of
                            [] -> if heur_fn gm board 'A' >= heur_fn gm board 'B' then 1 else 2
                            _  -> 0    
        



-- get keys with value v from gm
-- getKeys gm v = filter (==v) gm

-- countConnected: find the number of pieces that are touching another piece of the same color on the board https://www.cs.swarthmore.edu/~bryce/cs63/s16/labs/hex.html
-- for all pieces, +1 if they are touching another piece
-- a piece is touching another piece if a coordinate belonging to a color is a neighbour of that piece
countConnected :: (M.GridMap gm v, Ord (G.Index g), Eq v, Num a, G.Grid g, G.Index (M.BaseGrid gm v) ~ G.Index g) => gm v -> g -> v -> a
countConnected gm board color = sum $ map (\x -> countNeighbor x) coordinates
        where getCommonColors _ v = v == color -- get kv pairs in grid map that have the same value as color
              coordinates = M.keys $ M.filterWithKey getCommonColors gm -- get list of coordinates belonging to that color (get keys with value v from gm)
              noNeighborIsSameColor = null . flip intersect coordinates . G.neighbours board -- returns boolean on whether a neighbour of the current point is a dot of the same color
              countNeighbor me = if noNeighborIsSameColor me then 0 else 1


-- ask user for how much time AI should spend
askTime :: IO Integer
askTime = do
    putStrLn "How deep should the AI have to compute each move? (Default is 2)"
    response <- getLine
    case response of
        "" -> return 2
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

askParallel :: IO Integer
askParallel = do
    putStrLn "Should we run it parallel? (0=no, 1=yes)"
    response <- getLine
    case response of
        "" -> return 0
        s -> case readMaybe s of 
                Just n -> return n
                Nothing -> askParallel

main :: IO ()
main = do 
    max_depth <- askTime
    pre_size <- askSize
    is_parallel <- askParallel
    let size = fromIntegral pre_size
    let hex_b = paraHexGrid size size
    let hex_grid = lazyGridMap hex_b (take (size*size) (repeat '-'))

    if is_parallel == 1 then
        do
            par_playGame size hex_grid hex_b 'A' (countConnected) (basicGameOver) max_depth
            return ()

    else
        do
            playGame size hex_grid hex_b 'A' (countConnected) (basicGameOver) max_depth
            return ()

      


draw :: (M.GridMap gm Char, G.Index (M.BaseGrid gm Char) ~ (Int, Int)) => Int -> gm Char -> String
draw size board = unlines [line y | y <- [0..size]] where
    line 0 = (' ':) $ take size ['A'..] >>= (:" ") --draw top legend
    line y = replicate y ' ' ++ --white space for formatting
             replicate (length $ show y) '\b' ++  -- delete extra spaces
             show y ++ -- print current number
             concat [[' ', cell x (y - 1)] | x <- [0..size-1]] -- actual line info
    cell x y = case M.lookup (x, y) board of
                    Just a -> a -- change this w actual data type used by value (this one needs FlexibleContexts)
                    Nothing -> '-'

