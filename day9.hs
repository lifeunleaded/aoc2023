import System.IO  
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (nub)

readSInt :: String -> Int
readSInt = read

allZeros :: [Int] -> Bool
allZeros [] = False
allZeros (i:is) = (i==0) && (length(nub(i:is))==1)

getNext :: [Int] -> Int
getNext is = if (allZeros derivedseq) then (last is) else ((last is) + (getNext derivedseq))
  where derivedseq = zipWith (-) (drop 1 is) (init is)

main = do
        handle <- openFile "inputs/day9/input" ReadMode
        contents <- hGetContents handle
        let list = map (\x -> map readSInt x) (map (splitOn " ") (lines contents))
        print "Part 1"
        print(sum(map getNext list))
