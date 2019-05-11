module Pin where

data Pin = FIRST | SECOND | THIRD | FOURTH | FIFTH | SIXTH | SEVENTH | EIGHTH | NINTH | TENTH
  deriving (Show, Enum)

class EnumDataPointer a where
  getData :: Int -> a
  getColNum :: a -> Int

instance EnumDataPointer Pin where
  getData n = ( toEnum n :: Pin )
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

getRemainsPins :: [Int] -> [Bool] -> [Bool]
getRemainsPins [x] remainsTable =
  setUpPin x remainsTable
getRemainsPins (x:remains) remainsTable =
  getRemainsPins remains (setUpPin x remainsTable)

setUpPin :: Int -> [Bool] -> [Bool]
setUpPin x remainsTable =
  (take (x - 1) $ remainsTable) ++ [True] ++ (drop x remainsTable)