module ListHandling
    ( listPrinter
    ) where

import Data.List (foldl')

listPrinter :: IO ()
listPrinter = do
  print sampleList
  print $ map addTen sampleList
  print $ foldl' (+) 0 sampleList

addTen :: Int -> Int
addTen x = x + 10

sampleList :: [Int]
sampleList = [ 1, 3, 5, 6, 23, 25, 11, 10, 7, 22, 31]
