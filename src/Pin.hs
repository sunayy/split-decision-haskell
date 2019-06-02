module Pin (
  isSplit
) where

import Text.Regex.Posix

data Pin = FIRST | SECOND | THIRD | FOURTH | FIFTH | SIXTH | SEVENTH | EIGHTH | NINTH | TENTH
  deriving (Show, Enum)

getData :: Int -> Pin
getData n = ( toEnum (n - 1) :: Pin )

getColNum :: Pin -> Int
getColNum pin =
    case pin of
      SEVENTH -> 0
      FOURTH  -> 1
      SECOND  -> 2
      EIGHTH  -> 2
      FIRST   -> 3
      FIFTH   -> 3
      THIRD   -> 4
      NINTH   -> 4
      SIXTH   -> 5
      _       -> 6

getColNumById :: Int -> Int
getColNumById = getColNum . getData

convertToColTable :: [Int] -> [Bool]
convertToColTable args = (flip getRemainedPins) (replicate 7 False) $ map getColNumById args

getRemainedPins :: [Int] -> [Bool] -> [Bool]
getRemainedPins [x] remainsTable = setUpPin x remainsTable
getRemainedPins (x:remains) remainsTable = getRemainedPins remains (setUpPin x remainsTable)

setUpPin :: Int -> [Bool] -> [Bool]
setUpPin x remainsTable = (take x $ remainsTable) ++ [True] ++ (drop (x + 1) remainsTable)

mapToStringFromRemainedPins :: [Bool] -> String
mapToStringFromRemainedPins = map (\b -> if b then '1' else '0')

mapToInt :: [String] -> [Int]
mapToInt = map (\s -> read s :: Int)

argsToColTable :: [String] -> [Bool]
argsToColTable = convertToColTable . mapToInt

argsToRemainedString :: [String] -> String
argsToRemainedString = mapToStringFromRemainedPins . argsToColTable

isSplit :: [String] -> Bool
isSplit args = (argsToRemainedString args) =~ ".*10+1.*"