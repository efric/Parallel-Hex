{-# LANGUAGE GADTs, FlexibleContexts #-} 

module Main where
import Math.Geometry.Grid.Hexagonal ( paraHexGrid )
import Math.Geometry.Grid
import qualified Math.Geometry.GridMap as M
import Math.Geometry.GridMap.Lazy ( lazyGridMap )
import Text.Read
import Data.List

computeValidMoves grid_map = (get_positions grid_map) \\ (used_tiles grid_map)
    where get_positions grid_map = indices $ M.toGrid grid_map
          used_tiles grid_map = [fst x | x <- M.toList grid_map]

-- countConnected color grid_map = fst $ countConnectedRec (head same_color) same_color grid_map 0
--     where same_color = getSameColor color (M.toList grid_map)


-- countConnectedRec :: (Eq a, Eq b) => (a, b) -> [(a, b)] -> g -> Int -> (Int, [(a, b)])
-- countConnectedRec node remaining_of_same_color grid_map curr_count = let new_remaining = (remaining_of_same_color \\ (node : same_neighbours))
--                                                                 in (curr_count + length same_neighbours, new_remaining) 
--                                                                 where same_neighbours = getSameColor (snd node) (neighbours grid_map node) `intersect` remaining_of_same_color

getSameColor :: Eq b => b -> [(a, b)] -> [a]
getSameColor color grid_list = [fst x | x <- grid_list, snd x == color]



p1Max grid_map heuristic depth_limit curr_depth = pxMax "A" grid_map heuristic depth_limit curr_depth


p2Max grid_map heuristic depth_limit curr_depth = pxMax "B" grid_map heuristic depth_limit curr_depth



-- TODO: Types dont match since minimax_find_min and find_max return a tuple, but pxMax should right now is returning just the heuristic value. This is wrong.
pxMax color grid_map heuristic depth_limit curr_depth = if curr_depth == depth_limit 
                                                        then ( heuristic anti_color grid_map, (-1,-1) )
                                                        else 
                                                            case color of
                                                                "A" -> minimax_find_min (map (\move -> (fst (p2Max (M.insert move color grid_map) heuristic depth_limit (curr_depth + 1)), move) )  (computeValidMoves grid_map) ) 0 
                                                                "B" -> minimax_find_max (map (\move -> (fst (p1Max (M.insert move color grid_map) heuristic depth_limit (curr_depth + 1)), move) )  (computeValidMoves grid_map) ) 0
                                                        where 
                                                              anti_color = case color of 
                                                                                "A" -> "B"
                                                                                "B" -> "A"

minimax_find_min []     curr_max = curr_max
minimax_find_min (m:ms) curr_max = if (fst m) < (fst curr_max) then minimax_find_min ms m else minimax_find_min ms curr_max 
minimax_find_max []     curr_max = curr_max
minimax_find_max (m:ms) curr_max = if (fst m) > (fst curr_max) then minimax_find_min ms m else minimax_find_min ms curr_max 


minimax grid_map depth_limit heuristic = map (\move -> p2Max (M.insert move "A" grid_map) heuristic depth_limit 1) $ computeValidMoves grid_map





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
     time <- askTime
     size <- askSize
     putStrLn (draw 26 hex_b) -- hardcoded example
     putStrLn (show (computeValidMoves (hex_b)))
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

hex_b = lazyGridMap (paraHexGrid 11 11) ['a', 'b', 'c']
