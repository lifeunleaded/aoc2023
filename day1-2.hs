import System.IO  
import Control.Monad
import Data.Char (isDigit)
import Data.Maybe (maybeToList)


readMaybeSInt :: String -> [Maybe Int]
readMaybeSInt ('o':'n':'e':ss) = [Just 1] <> readMaybeSInt (['n','e'] <> ss)
readMaybeSInt ('t':'w':'o':ss) = [Just 2] <> readMaybeSInt (['w','o'] <> ss)
readMaybeSInt ('t':'h':'r':'e':'e':ss) = [Just 3] <> readMaybeSInt (['h','r','e','e'] <> ss)
readMaybeSInt ('f':'o':'u':'r':ss) = [Just 4] <> readMaybeSInt (['o','u','r'] <> ss)
readMaybeSInt ('f':'i':'v':'e':ss) = [Just 5] <> readMaybeSInt (['i','v','e'] <> ss)
readMaybeSInt ('s':'i':'x':ss) = [Just 6] <> readMaybeSInt (['i','x'] <> ss)
readMaybeSInt ('s':'e':'v':'e':'n':ss) = [Just 7] <> readMaybeSInt (['e','v','e','n'] <> ss)
readMaybeSInt ('e':'i':'g':'h':'t':ss) = [Just 8] <> readMaybeSInt (['i','g','h','t'] <> ss)
readMaybeSInt ('n':'i':'n':'e':ss) = [Just 9] <> readMaybeSInt (['i','n','e'] <> ss)
readMaybeSInt (c:ss) = [readMaybeCInt c] <> readMaybeSInt ss
readMaybeSInt [] = []

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
  where ints = getInts(readMaybeSInt s)


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
        print (length(list))
        print (length(ints))
        print (list!!0)
        print (ints!!0)
        print (list!!273)
        print (ints!!273)
        print (last(list))
        print (last(ints))
        print sumints
