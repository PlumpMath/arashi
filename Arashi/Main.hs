{-# LANGUAGE ScopedTypeVariables #-}

import System.Console.CmdTheLine
import System.Environment (getArgs)
import Control.Concurrent (forkIO, threadDelay)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Concurrent.Chan
import Data.IORef
import Control.Applicative ((<$>), (<*>), pure)

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

saveEntriesThread :: Int -> IORef Entries -> IO ()
saveEntriesThread i entriesRef = do
    threadDelay i
    entries <- readIORef entriesRef
    putStrLn $ "store: saving " ++ show (S.size entries) ++ " entries"
    L8.writeFile "all_posts.edn" $ encode entries
    saveEntriesThread i entriesRef

decodeFile :: (FromEDN v) => FilePath -> IO (Maybe v)
decodeFile p = readFile p >>= return . decode . convertString

fetchOne :: String -> IO ()
fetchOne url = mapM_ print =<< fetchEntries url

fetchAll :: IO ()
fetchAll = undefined

server :: Int -> Int -> Int -> IO ()
server n f s = do
    Just (Config urls) <- decodeFile "config.edn"
    Just (entries :: Entries) <- decodeFile "all_posts.edn"
    entriesRef <- newIORef entries
    entriesChan <- newChan
    runPeriodically 10 $ map (\url -> (fetchEntriesThread entriesChan url, f)) urls
    forkIO $ collectEntriesThread entriesRef entriesChan
    forkIO $ saveEntriesThread s entriesRef
    runServer 3001 entriesRef

-- console ui

numThreads :: Term Int
numThreads = value $ opt 10 $ optInfo ["num-threads", "n"]

fetchInterval :: Term Int
fetchInterval = fmap (* 60) . value $ opt (2 * 60) $ optInfo ["fetch-interval", "f"]

storeInterval :: Term Int
storeInterval = fmap (* 60) . value $ opt (10) $ optInfo ["store-interval", "s"]

info :: TermInfo
info = defTI { termName = "arashi", version = "0.0.1" }

command :: Term String
command = value $ pos 0 "run-server" $ posInfo { posName = "<cmd>", posDoc = "fetch-one, fetch-all or run-server" }

main :: IO ()
main = runChoice (pure $ return (), info) [
    (fetchOne <$> (required $ pos 0 Nothing posInfo), defTI { termName = "fetch-one" }),
    (pure fetchAll, defTI { termName = "fetch-all" }),
    (server <$> numThreads <*> fetchInterval <*> storeInterval, defTI { termName = "run-server" })
    ]
