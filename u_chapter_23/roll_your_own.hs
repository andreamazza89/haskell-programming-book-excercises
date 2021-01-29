module RollYourOwn where

import System.Random
import RandomExample
import RandomExample2
import Control.Monad.Trans.State

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = rollsToGetN 20

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) =
             randomR (1, 6) gen
        in go (sum + die)
              (count + 1) nextGen

rollsCountLogged :: Int -> State StdGen (Int, [Die])
rollsCountLogged n =
  ((,) <$> length <*> id)
    <$> myTakeWhile ((< n) . sum . fmap dieToInt)
    <$> rollForever

myTakeWhile :: ([a] -> Bool) -> [a] -> [a]
myTakeWhile f xs = go f xs []
  where
    go :: ([a] -> Bool) -> [a] -> [a] -> [a]
    go _ [] result = result
    go shouldContinue remaining@(x:xs) result
      | shouldContinue result = go shouldContinue xs (result ++ [x])
      | otherwise = result







