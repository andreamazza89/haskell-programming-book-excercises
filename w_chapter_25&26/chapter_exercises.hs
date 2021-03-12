module ChapterExercises where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity

rDec :: Num a => Reader a a
rDec =
  reader (flip (-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  n <- ask
  liftIO $ print ("Hi: " ++ show n)
  return $ n + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  n <- get
  liftIO $ print $ "The input was " ++ show n
  put $ n + 1
  return $ show n

-- fix the code

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn ("Good, was very excite: " ++ e)
