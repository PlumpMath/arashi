import System.Environment (getArgs)
import Control.Monad (forM_)

import Arashi.Core (fromFetchedFeed)

main :: IO ()
main = do
    [url] <- getArgs
    Just entries <- fromFetchedFeed url
    forM_ entries $ \entry ->
        print entry
