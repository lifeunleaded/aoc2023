import System.IO  
import Control.Monad
import Data.List.Split (split, splitWhen)
import Data.List (nub)
import Data.Char (isDigit)

winCount :: [String] -> [Bool]
winCount ss = map (`elem` wins) mine
  where
    wins = takeWhile (/="|") ss
    mine = takeWhile (/="|") (reverse ss)

winCount' :: [String] -> Int
winCount' ss = length (filter (==True) (winCount ss))

getWinsSum :: [Int] -> [Int] -> Int
getWinsSum [] _ = 0
getWinsSum _ [] = 0
getWinsSum (c:cs) (a:as) = 1 + (getWinsSum (take c as) as) + (getWinsSum cs as)

main = do
        handle <- openFile "inputs/day4/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let cards = map (drop 2) (map (filter (/="")) (map (splitWhen (==' ')) list))
        let wincount = map winCount' cards
        print("Part 2")
        print(getWinsSum wincount wincount)

        
        
