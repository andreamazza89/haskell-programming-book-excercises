{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
      counts :: IORef (M.Map Text Integer)
    , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)

type Handler =
  ActionT Text (ReaderT Config IO)

getCounts :: Config -> IO (M.Map Text Integer)
getCounts config =
  readIORef (counts config)

getCountFor :: Text -> Config -> IO Integer
getCountFor key config = do
  return
  . fromMaybe 0
  . M.lookup key
  =<< getCounts config

increaseCountFor :: Text -> Config -> Integer -> IO ()
increaseCountFor key config newCount = do
  writeIORef (counts config)
  . M.insert key newCount
  =<< getCounts config


buildKey :: Config -> Text -> Text
buildKey config unprefixed =
  mappend (prefix config) unprefixed

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask
    let key' = buildKey config unprefixed
    count <- liftIO $ getCountFor key' config
    liftIO $ increaseCountFor key' config (count + 1)
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show count
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app
