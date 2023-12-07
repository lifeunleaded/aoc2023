import System.IO  
import Control.Monad
import Data.List.Split (splitOn)
--import Data.Char (isDigit)
import Data.List (nub, sort)

data Card = NoCard | Two | Three | Four
     | Five | Six | Seven | Eight | Nine | Ten 
     | Jack | Queen | King | Ace
    deriving (Enum, Eq, Ord, Show)

data HandType = NoHand | High | Pair | TwoPair | ThreeEq | House | FourEq | FiveEq
  deriving (Enum, Eq, Ord, Show)

data Hand = Hand { handtype :: HandType, cards :: [Card] } deriving (Eq, Show)

instance Ord Hand where
  compare hand1 hand2 = if ((handtype hand1) == (handtype hand2))
    then compare (cards hand1) (cards hand2)
    else compare (handtype hand1) (handtype hand2)

readSInt :: String -> Int
readSInt = read

readHand :: [Card] -> Hand
readHand cs = Hand { handtype = readHandType cs, cards = cs }

readHandType :: [Card] -> HandType
readHandType cs
  | length(nub(cs)) == 1 = FiveEq
  | length(nub(take 4 (sort cs))) == 1 || length(nub(drop 1 (sort cs))) == 1 = FourEq
  | (length(nub(take 3 (sort cs))) == 1 && length(nub(drop 3 (sort cs))) == 1) ||
    (length(nub(take 2 (sort cs))) == 1 && length(nub(drop 2 (sort cs))) == 1) = House
  | length(nub(take 3 (sort cs))) ==  1
    || length(nub(drop 2 (sort cs))) == 1
    || length(nub(take 3 (drop 1 (sort cs)))) == 1 = ThreeEq
  | length(nub(cs)) == 3 = TwoPair
  | length(nub(cs)) == 4 = Pair
  | otherwise = High

readCard :: Char -> Card
readCard '2' = Two
readCard '3' = Three
readCard '4' = Four
readCard '5' = Five
readCard '6' = Six
readCard '7' = Seven
readCard '8' = Eight
readCard '9' = Nine
readCard 'T' = Ten
readCard 'J' = Jack
readCard 'Q' = Queen
readCard 'K' = King
readCard 'A' = Ace
readCard _ = NoCard

main = do
        handle <- openFile "inputs/day7/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let games = map (splitOn " ") list -- [["32T3K","765"],["T55J5","684"],...
        let parsedgames = map (\x -> ((readHand (map readCard (x!!0))),readSInt(x!!1))) games
        let orderedbids = map snd (sort parsedgames)
        let ranks = [1 .. (length(list))]        
        print("Part 1")
        print(sum(zipWith (*) orderedbids ranks))

