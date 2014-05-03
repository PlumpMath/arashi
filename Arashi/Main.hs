import System.Environment (getArgs)
import Control.Monad (forM_)

import Data.EDN (decode)
import Data.String.Conversions (convertString)

import Arashi.Core (fromFetchedFeed)
import Arashi.Config
import Arashi.HTML

main :: IO ()
main = do
    args <- getArgs
    case args of
        [url] -> do
            feed <- feedFromURI url
            Just entries <- fromFetchedFeed feed
            forM_ entries $ \entry ->
                print entry
        _ -> do
            Just (Config uris) <- readFile "config.edn" >>= return . decode . convertString
            forM_ uris putStrLn
