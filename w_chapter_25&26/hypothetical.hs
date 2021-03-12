module Hypothetical where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- let's say I have an int as the environment for the reader.
-- I want to wrap a maybe in it to have a string "yes, higher than 5" or a Nothing if it isn't higher than 5

isHigherThan5 :: ReaderT Int Maybe String
isHigherThan5 =
  ReaderT $ \r -> do
    if r > 5 then
      Just "hi"
    else
      Nothing


main :: Int -> Maybe String
main i =
  runReaderT isHigherThan5 i

isHigherThan5' :: MaybeT (Reader Int) String
isHigherThan5' =
  MaybeT $ do
    numb <- ask
    if numb > 5 then
      return (Just "hi")
    else
      return Nothing

main' :: Int -> Maybe String
main' =
  runReader . runMaybeT $ isHigherThan5'
