module Arashi.Schedule where

import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.SSem as Sem
import Control.Exception (SomeException, try)

-- a simple schedule
--
-- - fetch periodically (configurable, 8 hours, probably)
-- - into a set of entries
-- - fetching should be parallel
-- - sleeping/waiting should not block other threads

type DelayInS = Int

runPeriodically :: Int -> [(IO a, DelayInS)] -> IO ()
runPeriodically numThreads actions = do
    sem <- Sem.new numThreads
    forM_ actions $ \(action, delay) ->
        forkIO $ run_ sem action delay
  where run_ sem action delay = do
            res <- try $ Sem.withSem sem action
            case res of
                Left e -> print (e :: SomeException)
                _ -> return ()
            threadDelay $ delay * 1000 * 1000
            run_ sem action delay
