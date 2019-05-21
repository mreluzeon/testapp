module Lib where

import qualified Control.Monad as C
import qualified System.Random as R
import qualified Data.Array.IO as AR
import Data.Text.Lazy (pack)

import Types

chunkBy :: Int -> [a] -> [[a]]
chunkBy _ []   = []
chunkBy i coll = chunkBy' (splitAt i coll) []
  where
    chunkBy' (a,[]) res = res ++ [a]
    chunkBy' (a,rst) res = chunkBy' (splitAt i rst) (res ++ [a])

getQuestions :: [[Question]] -> String -> Int -> [Question]
getQuestions qss cs n = concat $ map mapper $ zip qss css
  where
    mapper (qs, cs) = map ((!!) qs . (+ (-65)) . fromEnum) cs
    css = chunkBy n cs

checkAnswers :: [Question] -> [String] -> [Bool]
checkAnswers qs as = map (uncurry (==)) (zip (map (head . answers) qs) as)

countAnswers :: [Bool] -> Int
countAnswers = length . filter id

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  C.forM [1..n] $ \i -> do
    j <- R.randomRIO (i,n)
    vi <- AR.readArray ar i
    vj <- AR.readArray ar j
    AR.writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (AR.IOArray Int a)
    newArray n xs = AR.newListArray (1,n) xs

shuffleAnswers :: [Question] -> IO [Question]
shuffleAnswers = C.mapM mapper
  where
    mapper (Question title qs) = Question title <$> (shuffle qs)

getVariant :: Int -> Int -> IO String
getVariant n m = C.forM [1..n] $ const $ toEnum <$> (R.randomRIO (65,64+m))
