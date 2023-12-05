import System.IO  
import Control.Monad
import Data.List.Split (splitWhen)
import Data.List (find)

readSInt :: String -> Int
readSInt s = read s

getListPair :: String -> String -> [String] -> [[Int]]
getListPair end start ss = map (map readSInt) (map (splitWhen (==' ')) (filter (/="") (tail (takeWhile (/=end) (dropWhile (/=start) ss)))))

getMapFromListPair :: [[Int]] -> [Int] -> [(Int,Int)]
getMapFromListPair is cand = filter (\(f,s) -> f `elem` cand) (zip (concat(map (\y -> (map (\x -> x + (y!!1)) [0 .. (y!!2)-1])) is)) (concat(map (\y -> (map (\x -> x + (y!!0)) [0 .. (y!!2)-1])) is)))

getPaths :: [(Int,Int)] -> Int -> Int
getPaths ps i = case (find (\(f,s) -> f == i) ps) of
  Just (f,s) -> s
  Nothing -> i

getPathsF :: Int -> [(Int,Int)] -> Int
getPathsF i ps = getPaths ps i

main = do
        handle <- openFile "inputs/day5/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let seeds = map (readSInt) (tail (splitWhen(==' ') (list!!0)))
        let seedtosoilmap = getMapFromListPair (getListPair "soil-to-fertilizer map:" "seed-to-soil map:" list) seeds
        let soils = map (getPaths seedtosoilmap) seeds
        let soiltofertilizermap = getMapFromListPair (getListPair "fertilizer-to-water map:" "soil-to-fertilizer map:" list) soils
        let fertilizers = map (getPaths soiltofertilizermap) soils
        let fertilizertowatermap = getMapFromListPair (getListPair "water-to-light map:" "fertilizer-to-water map:" list) fertilizers
        let waters = map (getPaths fertilizertowatermap) fertilizers
        let watertolightmap = getMapFromListPair (getListPair "light-to-temperature map:" "water-to-light map:" list) waters
        let lights = map (getPaths watertolightmap) waters
        let lighttotempmap = getMapFromListPair (getListPair "temperature-to-humidity map:" "light-to-temperature map:" list) lights
        let temps = map (getPaths lighttotempmap) lights
        let temptohumidmap = getMapFromListPair (getListPair "humidity-to-location map:" "temperature-to-humidity map:" list) temps
        let humids = map (getPaths temptohumidmap) temps
        let humidtolocmap = getMapFromListPair (map (map readSInt) (map (splitWhen (==' ')) (filter (/="") (tail (dropWhile (/="humidity-to-location map:") list))))) humids
        let locs = map (getPaths humidtolocmap) humids

        print("Part 1")
        print(minimum locs)
