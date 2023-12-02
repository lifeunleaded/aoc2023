import System.IO  
import Control.Monad
import Data.List.Split
import Data.Char (isDigit)

data ColorCount = ColorCount { redcount :: Int
                             , greencount :: Int
                             , bluecount :: Int
                  } deriving Show

instance Semigroup ColorCount where
  (<>) c1 c2 = ColorCount { redcount = maximum([redcount c1, redcount c2])
                          , greencount = maximum([greencount c1, greencount c2])
                          , bluecount = maximum([bluecount c1, bluecount c2]) }

instance Monoid ColorCount where
  mempty = ColorCount { redcount = 0
                      , greencount = 0
                      , bluecount = 0 }
  mappend = (<>)

data Game = Game { gamenumber :: Int
            , colorcounts :: ColorCount} deriving Show

readInt :: Char -> Int
readInt c = read [c]

readSInt :: String -> Int
readSInt s = read s

parseSet :: String -> ColorCount
parseSet (' ':xs) = parseSet xs
parseSet s = case (isDigit (head(s))) of
  True -> do
    let l = splitOn " " s
    let texttail = l!!1
    let numhead = l!!0
    case texttail of 
      "red" -> ColorCount { redcount = readSInt numhead, greencount = 0, bluecount = 0 }
      "green" -> ColorCount { redcount = 0, greencount = readSInt numhead, bluecount = 0 }
      "blue" -> ColorCount { redcount = 0, greencount = 0, bluecount = readSInt numhead }
      _ -> ColorCount { redcount = 0, greencount = 0, bluecount = 0 }
  False -> ColorCount { redcount = 0, greencount = 0, bluecount = 0 }

parseDraw :: String -> ColorCount
parseDraw s = foldl1 (<>) (map parseSet (splitOn "," s))

parseGame :: String -> ColorCount
parseGame s = foldl1 (<>) (map parseDraw (splitOn ";" s))

parseGameNumber :: String -> Int
parseGameNumber ('G':'a':'m':'e':' ':is) = readSInt is

parseGameFromLine :: String -> Game
parseGameFromLine s = Game { gamenumber = parseGameNumber head
                           , colorcounts = parseGame tail } where
  head = l!!0
  tail = l!!1
  l = splitOn ":" s

possibleGame :: ColorCount -> Game -> Bool
possibleGame c g = (redcount c >= redcount (colorcounts g))
  && (bluecount c >= bluecount (colorcounts g))
  && (greencount c >= greencount (colorcounts g))

possibleGames :: [Game] -> ColorCount -> [Game]
possibleGames gs c = filter (possibleGame c) gs

sumGameNums :: [Game] -> Int
sumGameNums gs = sum (map gamenumber gs)

minConstrain :: Game -> ColorCount
minConstrain g = ColorCount { redcount = redcount (colorcounts g)
                            , greencount = greencount (colorcounts g)
                            , bluecount = bluecount (colorcounts g) }

powerOfCc :: ColorCount -> Int
powerOfCc c = redcount c * greencount c * bluecount c

main = do
        handle <- openFile "inputs/day2/input" ReadMode
        contents <- hGetContents handle
        let list = lines contents
        let games = map parseGameFromLine list
        let testc = ColorCount { redcount = 12, greencount = 13, bluecount = 14 }
        let pgames = possibleGames games testc
        print("Part 1")
        print(sumGameNums pgames)
        print("Part 2")
        print(sum(map powerOfCc (map minConstrain games)))
