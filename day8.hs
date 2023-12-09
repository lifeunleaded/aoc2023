import System.IO  
import Control.Monad
import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

lineToNode :: [String] -> [String]
lineToNode ss = if length(ss) == 4 then [(ss!!0), (take 3 (drop 1 (ss!!2))), (take 3 (ss!!3))] else []

first :: [String] -> String
first [] = []
first (x:xs) = x

second :: [String] -> String
second [] = []
second (x:[]) = []
second (x:y:_) = y

third :: [String] -> String
third [] = []
third (x:[]) = []
third (x:y:[]) = []
third (x:y:z:_) = z

-- index :: String -> [String] -> Int
-- index s ss = case elemIndex s ss of
--   Just x -> x
--   Nothing -> -1

nextNode :: [[(String,String)]] -> Int -> String -> String -> [Char] -> Int
nextNode _ _ _ _ [] = error "Goal not found"
nextNode [lefts, rights] count start goal dirs =
  if (start == goal)
  then count
  else nextNode [lefts, rights] (count+1) next goal (tail dirs)
  where next = case dir of
          'L' -> fromJust (lookup start lefts)
          'R' -> fromJust (lookup start rights)
        dir = head dirs
        
main = do
        handle <- openFile "inputs/day8/input2" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let dirs = list!!0
        let entries = map lineToNode (map (splitOn " ") (drop 2 list))
        let nodelist = [(map first entries), (map second entries), (map third entries)]
        let lefts = zip (nodelist!!0) (nodelist!!1)
        let rights = zip (nodelist!!0) (nodelist!!2)
        let start = "AAA"
        let goal = "ZZZ"
        let res = nextNode [lefts,rights] 0 start goal (cycle dirs)
        print "Part 1"
        print "Start:"
        print start
        print "Goal:"
        print goal
        print res
