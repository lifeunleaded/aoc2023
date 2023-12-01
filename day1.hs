import System.IO  
import Control.Monad
-- import Text.Read (readMaybe)
import Data.Char (isDigit)
import Data.Maybe (maybeToList)

readMaybeCInt :: Char -> Maybe Int
readMaybeCInt c = if isDigit c then Just (readCInt c) else Nothing

readCInt :: Char -> Int
readCInt c = read [c]

-- getMaybeInts :: String -> [Maybe Int]
-- getMaybeInts s = map readMaybeInt s

getInts :: [Maybe Int] -> [Int]
getInts [] = []
getInts (x:xs) = case x of
  Just i -> [i] <> getInts xs
  Nothing -> getInts xs

getFirstAndLastInt :: String -> (Int,Int)
getFirstAndLastInt s = (ints!!0,last(ints))
  where ints = getInts(map readMaybeCInt s)


showIntPair :: (Int,Int) -> String
showIntPair (a,b) = show a ++ show b

readInt :: String -> Int
readInt s = read s
  
main = do  
        handle <- openFile "inputs/day1/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let ints = map readInt(map showIntPair (map getFirstAndLastInt list))
        let sumints = sum ints
        print sumints
