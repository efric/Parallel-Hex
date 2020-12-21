{-# LANGUAGE GADTs, FlexibleContexts #-} 

module Main where
import Math.Geometry.Grid.Hexagonal
import qualified Math.Geometry.GridMap as M
import Math.Geometry.GridMap.Lazy
import Text.Read

-- ask user for how much time AI should spend
askTime = do
    putStrLn "How many seconds should the AI have to compute each move? (Default is 0.2)"
    response <- getLine
    case response of
        "" -> return 0.2
        s -> case readMaybe s of --parse input
            Just n -> return n --if number, return that number
            Nothing -> askTime --if it aint, ask again

-- ask user for how big the board should be 
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
