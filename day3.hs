import System.IO  
import Control.Monad
import Data.List.Split (split, splitWhen)
import Data.List (nub)
import Data.Char (isDigit)
import Data.Matrix

readTokens :: [(String, Bool)] -> [(Char, Bool)] -> [(String, Bool)]
readTokens acc [] = acc
readTokens [] (c:cs) = case (isDigit (fst c)) of
  True -> readTokens [([fst c],snd c)] cs
  False -> readTokens [] cs
readTokens acc (c:cs) = case (isDigit (fst c)) of
  True -> (init acc) ++ [(fst(last acc)++[fst c],snd(last acc) || snd c)] ++ readTokens newacc cs
  False -> (init acc) ++ [("",False)] ++ readTokens newacc2 cs
  where
    newacc = (init acc) ++ [(fst(last acc)++[fst c],snd(last acc) || snd c)]
    newacc2 = (init acc) ++ [("",False)]

readSInt' :: String -> Int
readSInt' s = read s

isCSym :: Char -> Bool
isCSym c = not((isDigit c) || (c=='.'))

getNeighbors :: Matrix Char -> (Int, Int) -> [(Int, Int)]
getNeighbors m (r,c)
  | (r == 1) && (c == 1) = [(1,1),(1,2),(2,1),(2,2)]
  | (r == 1) && (c < maxcols) = [(1,c-1),(1,c),(1,c+1),(2,c-1),(2,c),(2,c+1)]
  | (r == 1) && (c == maxcols) = [(1,c-1),(1,c),(2,c-1),(2,c)]
  | (r < maxrows) && (r > 1) && (c == 1) = [(r-1,1),(r-1,2),(r,1),(r,2),(r+1,1),(r+1,2)]
  | (r == maxrows) && (c == 1) = [(r-1,1),(r-1,2),(r,1),(r,2)]
  | (r == maxrows) && (c == maxcols) = [(r-1,c-1),(r-1,c),(r,c-1),(r,c)]
  | (r == maxrows) && (c < maxcols) && (c > 1) = [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c),(r,c+1)]
  | (r < maxrows) && (r > 1) && (c == maxcols) = [(r-1,c-1),(r-1,c),(r,c-1),(r,c),(r+1,c-1),(r+1,c)]
  | otherwise = [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)]
  where
    maxrows = nrows m
    maxcols = ncols m

inSymRange :: (Int,Int) -> Matrix Char -> Bool
inSymRange (r,c) m = foldl1 (||) (map (\(x,y) -> isCSym (m!(x,y)))(getNeighbors m (r,c)))

combine :: Char -> Bool -> (Char, Bool)
combine c b = (c,b)

addIfTrue :: (String, [Bool]) -> (String, [Bool]) -> (String, [Bool])
addIfTrue (s1, bs1) (s2, bs2)
  | (foldl1 (||) bs1) && (foldl1 (||) bs2) = (show(readSInt' s1 + readSInt' s2), [True])
  | (foldl1 (||) bs1) = (s1,bs1)
  | (foldl1 (||) bs2) = (s2,bs2)

main = do
        handle <- openFile "inputs/day3/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let m = fromLists list
        let symhits = zero (nrows m) (ncols m)
        print("Part 1")
        let symtouch = matrix (nrows m) (ncols m) (\(x,y) -> (inSymRange (x,y) m))
        let x = elementwise combine m symtouch
        let ls = toLists x
        let ls2 = map (filter (/=[])) (map (splitWhen (\x -> (not (isDigit(fst x))))) ls)
        let ls3 = map unzip (concat ls2)
        let sumpair = foldl addIfTrue ("0",[False]) ls3
        print(fst sumpair)

  
  
