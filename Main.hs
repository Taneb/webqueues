{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Map.Strict as M
import Data.UUID
import Network.HTTP.Types.Status
import System.Environment
import Web.Scotty

type QueueMapVar = TVar (M.Map UUID (TQueue ByteString))

-- | 'getUUID' parses the uuid parameter - looking in capture, then in form
-- data, then in query parameters - and parses it, failing with HTTP 400
-- ("Bad Request") on parse failure.
getUUID :: ActionM UUID
getUUID = param "uuid" >>= maybe invalidUUIDfailure return . fromASCIIBytes
  where
    invalidUUIDfailure = do
      status badRequest400
      fail "Invalid UUID"

-- | 'getAction' takes the next value from a queue and returns it with HTTP 200
-- ("OK"), or if the queue is empty or non-existent, returns nothing with HTTP
-- 204 ("No Content").
getAction :: QueueMapVar -> ActionM ()
getAction queuesRef = do
  uuid <- getUUID
  res <- liftIO . atomically $ do
    queues <- readTVar queuesRef
    maybe (return Nothing) tryReadTQueue $ M.lookup uuid queues
  maybe (status noContent204) (raw . fromStrict) res
          
-- | 'postAction' adds a value in the form field "value" to a queue, creating
-- the queue if necessary, and then returns HTTP 200 ("OK").
postAction :: QueueMapVar -> ActionM ()
postAction queuesRef = do
  uuid <- getUUID
  value <- param "value"
  liftIO . atomically $ do
    queues <- readTVar queuesRef
    case M.lookup uuid queues of
      Nothing -> do
        -- we have to make the queue!
        queue <- newTQueue
        writeTQueue queue value
        let newQueues = M.insert uuid queue queues
        writeTVar queuesRef newQueues
      Just queue -> writeTQueue queue value

-- | 'deleteAction' deletes a queue, if it exists, and returns HTTP 200 ("OK").
deleteAction :: QueueMapVar -> ActionM ()
deleteAction queuesRef = do
  uuid <- getUUID
  liftIO . atomically $ modifyTVar' queuesRef $ M.delete uuid

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        [] -> 9123
        [n] -> read n
  queuesRef <- newTVarIO M.empty
  scotty port $ do
    get "/:uuid" $ getAction queuesRef
    post "/:uuid" $ postAction queuesRef
    delete "/:uuid" $ deleteAction queuesRef
