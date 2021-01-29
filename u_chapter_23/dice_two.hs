module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample


rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  (,,) <$> rollDie <*> rollDie <*> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = take n <$> rollForever

rollForever :: State StdGen [Die]
rollForever = repeatM rollDie'

repeatM :: (Monad m) => m a -> m [a]
repeatM m = (:) <$> m <*> repeatM m