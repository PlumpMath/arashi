module Arashi.HTML where

import Data.Char (isAlpha)
import Data.List (isInfixOf)
import Text.URI (mergeURIStrings)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Data.String.Conversions (convertString)

import Arashi.Util (getHttp)

alternatesFromHTML :: (String -> Bool) -> String -> [String]
alternatesFromHTML typeLike html = map (onlyVal . fromAttrib "href") candidates
    where candidates = filter (tagOpenLit "link" (anyAttr onlyFeedAlternates)) tags
          tags = parseTags html
          onlyFeedAlternates (name, val) = onlyAlternates || onlyFeed
             where onlyAlternates = name == "rel" && filter isAlpha val == "alternate"
                   onlyFeed = name == "type" && typeLike (onlyVal val)
          onlyVal = filter (\v -> v /= '\\' && v /= '\"')

feedsFromHTML :: String -> [String]
feedsFromHTML = alternatesFromHTML ("xml" `isInfixOf`)

feedsFromURI :: String -> IO [String]
feedsFromURI uri = do
    res <- getHttp uri
    return . feedsFromHTML $ convertString res

feedFromURI :: String -> IO String
feedFromURI uri = do
    feeds <- feedsFromURI uri
    let feed = case feeds of
            (f:_) -> f
            _ -> uri
    return $ mergeURIStrings uri feed
