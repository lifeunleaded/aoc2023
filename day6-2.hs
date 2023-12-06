import System.IO  
import Control.Monad
import Data.List.Split (splitWhen)
import Data.Char (isDigit)

distance :: Int -> Int -> Int
distance _ 0 = 0
distance holdtime racetime = (racetime - holdtime) * holdtime

distances :: [Int] -> Int -> [Int]
distances holdtimes racetime = map (\x -> distance x racetime) holdtimes

beats :: [Int] -> Int -> [Int]
beats results record = filter (> record) results

readSInt :: String -> Int
readSInt s = read s

main = do
        handle <- openFile "inputs/day6/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let [racetime, record] = map (readSInt) (map (filter (isDigit)) list)
        let tests = [1 .. (racetime-1)]
        let results = distances tests racetime
        let options = beats results record

        print("Part 1")
        print(length(options))
