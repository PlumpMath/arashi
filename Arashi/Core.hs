{-# LANGUAGE OverloadedStrings #-}
module Arashi.Core where

import Text.Feed.Import (parseFeedString)
import Text.Feed.Query
import Text.Feed.Types (Feed, Item)

import Data.EDN
import Data.EDN.Types.Class (parseEDNv)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (msum)
import Data.Time
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as U8
import Network.HTTP.Conduit

data Entry = Entry {
    eUrl :: String,
    eTitle :: String,
    eVia :: Maybe String,
    eTimestamp :: UTCTime
} deriving (Show, Eq)

instance Ord Entry where
    compare = compareByUrl

compareByUrl :: Entry -> Entry -> Ordering
compareByUrl e1 e2 = eUrl e1 `compare` eUrl e2

compareByUrlAndTime :: Entry -> Entry -> Ordering
compareByUrlAndTime (Entry urlA _ _ timeA) (Entry urlB _ _ timeB) =
    case timeOrd of
        EQ -> urlA `compare` urlB
        _  -> timeOrd
  where timeOrd = timeA `compare` timeB

flipOrdering :: (Ord a) => (a -> a -> Ordering) -> a -> a -> Ordering
flipOrdering compare' x y =
    case x `compare'` y of
        LT -> GT
        EQ -> EQ
        GT -> LT

class Searchable a where
    matches :: String -> a -> Bool

instance Searchable Entry where
    matches search (Entry url title _ _) =
           search `isInfixOf` url
        || search `isInfixOf` title

instance ToEDN Entry where
    toEDN (Entry url title via time) =
        notag $ makeMap [
            "url" .= url,
            "title" .= title,
            "via" .= via,
            "timestamp" .= time
        ]

kw :: C8.ByteString -> Value
kw = Keyword

instance FromEDN Entry where
    parseEDNv (Map m) = Entry <$>
        m .:  kw "url" <*>
        m .:  kw "title" <*>
        m .:? kw "via" <*>
        m .:  kw "timestamp"

testEntry :: Entry
testEntry = Entry "http://te.st" "A (tiny) test" Nothing unixEpoch

unixEpoch :: UTCTime
unixEpoch = UTCTime (fromGregorian 1970 0 0) (secondsToDiffTime 0)

tinyTest :: Maybe Entry
tinyTest = decode . encode $ testEntry

fetchFeed :: String -> IO (Maybe Feed)
fetchFeed url = do
    req <- parseUrl url
    withManager $ \manager -> do
        res <- httpLbs req manager
        let body = U8.toString $ responseBody res
        return $ parseFeedString body

fromFetchedFeed :: String -> IO (Maybe [Entry])
fromFetchedFeed url = do
    t <- getCurrentTime
    (fmap . fmap $ fromFeed t) $ fetchFeed url

fromFeed :: UTCTime -> Feed -> [Entry]
fromFeed t f = map (fromItem (getFeedHome f) t) $ getFeedItems f

fromItem :: Maybe String -> UTCTime -> Item -> Entry
fromItem via t i = Entry url title via time
    where title = fromMaybe "" $ getItemTitle i
          url = fromMaybe "" $ getItemLink i
          time = fromMaybe t . msum $ map (\g -> g i >>= tryParseTime) [getItemPublishDateString, getItemDate]

tryParseTime :: String -> Maybe UTCTime
tryParseTime dateString = msum $
    map tryFormat [
        rfc822DateFormat,
        "%Y-%m-%dT%H:%M:%S%Q%z",
        "%Y-%m-%dT%H:%M:%SZ"
    ]
  where tryFormat format = parseTime defaultTimeLocale format dateString
