import System.IO  
import Control.Monad
import Data.List.Split (splitWhen, chunksOf)
import Data.List (elemIndex)

readSInt :: String -> Int
readSInt s = read s

getListPair :: String -> String -> [String] -> [[Int]]
getListPair end start ss = map (map readSInt) (map (splitWhen (==' ')) (filter (/="") (tail (takeWhile (/=end) (dropWhile (/=start) ss)))))
getListPair [] start ss = map (map readSInt) (map (splitWhen (==' ')) (filter (/="") (tail (dropWhile (/=start) ss))))



-- getFullListPair :: String -> String -> [String] -> [[Int]]
-- getFullListPair start end ss = [concat(map (\y -> (map (\x -> x + (y!!1)) [0 .. (y!!2)-1])) is), (concat(map (\y -> (map (\x -> x + (y!!0)) [0 .. (y!!2)-1])) is))] where is = getListPair end start ss

getRangeSpec :: String -> String -> [String] -> [[Int]]
getRangeSpec start end ss = getListPair end start ss

getNextM :: Int -> [Int] -> Int
getNextM src amap = if ((src >= (amap!!1)) && (src <= ((amap!!1) + (amap!!2))))
  then ((amap!!0)+(src-(amap!!1)))
  else src

getNextMM :: [[Int]] -> Int -> [Int]
getNextMM amap src = (map (getNextM src) amap)


getNext :: [[Int]] -> Int -> Int
getNext amap src = if (others == []) then src else (others!!0)
  where others = filter (/=src) (getNextMM amap src)

-- getNext :: Int -> [Int] -> [Int] -> Int
-- getNext query src dst = case elemIndex query src of
--   Just x -> dst!!x
--   Nothing -> query

-- getNextF :: [[Int]] -> Int -> Int
-- getNextF m query = getNext query src dst
--   where
--     src = m!!0
--     dst = m!!1

main = do
        handle <- openFile "inputs/day5/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let seeds' = map (readSInt) (tail (splitWhen(==' ') (list!!0)))
        let seeds = concat(map (\x -> [x!!0 .. (x!!0+x!!1-1)]) (chunksOf 2 seeds'))
        -- print "seed"
        -- print seeds
        let rs1 = getRangeSpec "seed-to-soil map:" "soil-to-fertilizer map:" list
--        print rs1
        let soils = map (getNext rs1) seeds
--        print "soil"
--        print soils
        let rs2 = getRangeSpec "soil-to-fertilizer map:" "fertilizer-to-water map:" list
--        print rs2
        let ferts = map (getNext rs2) soils
--        print "fert"
--        print ferts
        let rs3 = getRangeSpec "fertilizer-to-water map:" "water-to-light map:" list
--        print rs3
        let wat = map (getNext rs3) ferts
--        print "wat"
--        print wat
        let rs4 = getRangeSpec "water-to-light map:" "light-to-temperature map:" list
--        print rs4
        let light = map (getNext rs4) wat
--        print "light"
--        print light
        let rs5 = getRangeSpec "light-to-temperature map:" "temperature-to-humidity map:" list
--        print rs5
        let temp = map (getNext rs5) light
--        print "temp"
--        print temp
        let rs6 = getRangeSpec "temperature-to-humidity map:" "humidity-to-location map:" list
--        print rs6
        let hum = map (getNext rs6) temp
--        print "hum"
--        print hum
        let rs7 = getRangeSpec "humidity-to-location map:" "" list
--        print rs7
        let loc = map (getNext rs7) hum
--        print "loc"
--        print(loc)


        print("Part 2")
        print(minimum(loc))
