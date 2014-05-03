{-# LANGUAGE ScopedTypeVariables #-}
import System.Environment (getArgs)
import Control.Concurrent (forkIO, threadDelay)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Concurrent.Chan
import Data.IORef

import Data.EDN (FromEDN, decode, encode)
import Data.String.Conversions (convertString)
import qualified Data.ByteString.Lazy.Char8 as L8

import Arashi.Core
import Arashi.Config
import Arashi.HTML
import Arashi.Schedule
import Arashi.Render
import Arashi.Server

type Entries = Set Entry

insertIfNew :: (Ord a) => a -> Set a -> Set a
insertIfNew x s = if x `S.member` s then s else x `S.insert` s

insertAllNew :: (Ord a) => [a] -> Set a -> Set a
insertAllNew xs s= foldr insertIfNew s xs

modifyIORefR :: IORef a -> (a -> a) -> IO (a, a)
modifyIORefR ref f = do
    x <- readIORef ref
    let x' = f x
    writeIORef ref x'
    return (x, x')

collectEntriesThread :: IORef Entries -> Chan [Entry] -> IO ()
collectEntriesThread entries chan = do
    newEntries <- readChan chan
    case newEntries of
        ((Entry _ _ (Just via) _) :_) -> do
            (old, new) <- modifyIORefR entries (insertAllNew newEntries)
            let [oldS, newS] = map S.size [old, new]
            if oldS /= newS
            then putStrLn $ "collect: " ++ show (newS - oldS) ++ " new entries from " ++ via
                ++ " (total: " ++ show newS ++ ")"
            else putStrLn $ "collect: no new entries from " ++ via
        _ -> return ()
    collectEntriesThread entries chan

fetchEntries :: String -> IO [Entry]
fetchEntries url = feedFromURI url >>= fromFetchedFeed >>= \es -> return $ maybe [] id es

fetchEntriesThread :: Chan [Entry] -> String -> IO ()
fetchEntriesThread chan url = do
     entries <- fetchEntries url
     writeChan chan entries

saveEntriesThread :: IORef Entries -> IO ()
saveEntriesThread entriesRef = do
    threadDelay $ 1 * 60 * 1000 * 1000
    entries <- readIORef entriesRef
    putStrLn $ "store: saving " ++ show (S.size entries) ++ " entries"
    L8.writeFile "all_posts.edn" $ encode entries
    saveEntriesThread entriesRef

decodeFile :: (FromEDN v) => FilePath -> IO (Maybe v)
decodeFile p = readFile p >>= return . decode . convertString

main :: IO ()
main = do
    args <- getArgs
    case args of
        [url] -> mapM_ print =<< fetchEntries url
        _ -> do
            Just (Config urls) <- decodeFile "config.edn"
            Just entries <- decodeFile "all_posts.edn"
            entriesRef <- newIORef entries
            entriesChan <- newChan
            runPeriodically 10 $ map (\url -> (fetchEntriesThread entriesChan url, 1 * 60)) urls
            forkIO $ collectEntriesThread entriesRef entriesChan
            forkIO $ saveEntriesThread entriesRef
            runServer 4001 entriesRef
