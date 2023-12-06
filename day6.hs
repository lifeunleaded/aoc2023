import System.IO  
import Control.Monad
import Data.List.Split (splitWhen)

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
        let [racetimes, records] = map (\x -> map (readSInt) x) (map (drop 1) (map (filter (/="")) (map (splitWhen (==' ')) list)))
        let tests = map (\x -> [1 .. (x-1)]) racetimes
        let results = zipWith distances tests racetimes
        let options = zipWith beats results records
        let answer = foldl1 (*) (map (length) options)

        print("Part 1")
        print(answer)
