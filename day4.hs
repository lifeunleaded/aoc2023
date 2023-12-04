import System.IO  
import Control.Monad
import Data.List.Split (splitWhen)

winCount :: [String] -> [Bool]
winCount ss = map (`elem` wins) mine
  where
    wins = takeWhile (/="|") ss
    mine = takeWhile (/="|") (reverse ss)


main = do
        handle <- openFile "inputs/day4/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let cards = map (drop 2) (map (filter (/="")) (map (splitWhen (==' ')) list))
        let wins = map length (map (filter (==True)) (map winCount cards))
        let score = sum(map (\x -> 2^(x-1)) (filter (/=0) wins))
        print("Part 1")
        print(score)

        
        
