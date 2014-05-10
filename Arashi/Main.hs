{-# LANGUAGE ScopedTypeVariables #-}

import System.Console.CmdTheLine

import Control.Concurrent (forkIO, threadDelay)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Concurrent.Chan
import Data.IORef
import Control.Applicative ((<$>), (<*>), pure)
import System.IO (stdout)
import Data.Time (getCurrentTime)

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
    threadDelay $ i * 1000 * 1000
    entries <- readIORef entriesRef
    putStrLn $ "store: saving " ++ show (S.size entries) ++ " entries"
    L8.writeFile "all_posts.edn" $ encode entries
    saveEntriesThread i entriesRef

decodeFile :: (FromEDN v) => FilePath -> IO (Maybe v)
decodeFile p = L8.readFile p >>= return . decode

fetchOne :: String -> IO ()
fetchOne url = mapM_ print =<< fetchEntries url

fetchAll :: IO ()
fetchAll = do
    Just (Config urls) <- decodeFile "config.edn"
    Just entries <- decodeFile "all_posts.edn"
    feeds <- mapM (\url -> putStrLn ("fetching " ++ url) >> fetchEntries url) urls
    let newEntries = insertAllNew (concat feeds) entries
    putStrLn $ "got " ++ show (S.size newEntries - S.size entries) ++ " new entries (" ++ show (S.size newEntries) ++ " total)"
    L8.writeFile "all_posts.edn" $ encode entries

generateHtml :: Bool -> Int -> Int -> String -> IO ()
generateHtml reverse start count query = do
    Just es <- decodeFile "all_posts.edn"
    t <- getCurrentTime
    let entries = take count $ drop start $ filter (matches query) $ fromEntries es
    L8.hPut stdout $ indexHtml t entries

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

type Cmd = (Term (IO ()), TermInfo)

fetchOneCmd :: Cmd
fetchOneCmd = (cmd, info)
    where cmd = fetchOne <$> url
          url = required $ pos 0 Nothing $ posInfo { posName = "<url>"}
          info = defTI { termName = "fetch-one", termDoc = "Fetch and print entries from one feed given by <url>" }

fetchAllCmd :: Cmd
fetchAllCmd = (cmd, info)
    where cmd = pure fetchAll
          info = defTI { termName = "fetch-all", termDoc = "Fetch all entries from the feeds in config.edn" }

generateHtmlCmd :: Cmd
generateHtmlCmd = (cmd, info)
    where cmd = generateHtml <$> reverse <*> start <*> count <*> query
          start = value $ opt 0 $ optInfo ["start", "s"]
          count = value $ opt 100 $ optInfo ["count", "c"]
          query = value $ opt "" $ optInfo ["query", "q"]
          reverse = value . flag $ optInfo ["reverse", "r"]
          info = defTI { termName = "generate-html", termDoc = "Generate an HTML file of the feeds in all_posts.edn" }

runServerCmd :: Cmd
runServerCmd = (cmd, info)
    where cmd = server <$> numThreads <*> fetchInterval <*> storeInterval
          numThreads = value $ opt 10 $ optInfo ["num-threads", "n"]
          fetchInterval = fmap (* 60) . value $ opt (2 * 60) $ optInfo ["fetch-interval", "f"]
          storeInterval = fmap (* 60) . value $ opt (10) $ optInfo ["store-interval", "s"]
          info = defTI { termName = "run-server", termDoc = "Fetch all feeds periodically and serve the results with an embedded server" }

main :: IO ()
main = runChoice (pure $ return (), info) [fetchOneCmd, fetchAllCmd, generateHtmlCmd, runServerCmd]
    where info = defTI { termName = "arashi", version = "0.0.1" }
