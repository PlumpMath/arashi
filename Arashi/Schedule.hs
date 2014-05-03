module Arashi.Schedule where

import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)

-- a simple schedule
--
-- - fetch periodically (configurable, 8 hours, probably)
-- - into a set of entries
-- - fetching should be parallel
-- - sleeping/waiting should not block other threads

type DelayInS = Int

runPeriodically :: [(IO a, DelayInS)] -> IO ()
runPeriodically actions =
    forM_ actions $ \(action, delay) ->
        forkIO $ run_ action delay
  where run_ action delay = do
            action
            threadDelay $ delay * 1000 * 1000
            run_ action delay
