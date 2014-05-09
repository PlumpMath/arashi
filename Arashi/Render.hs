{-# LANGUAGE OverloadedStrings #-}
module Arashi.Render where

import Prelude hiding (head, id, div, span)
import Text.Blaze.Internal (stringValue)
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (span, style, title)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as L

import Text.URI (parseURI, uriRegName, uriScheme)
import Data.Time (UTCTime)
import Data.Time.Format.Human (humanReadableTime')
import Arashi.Core

indexHtml :: UTCTime -> [Entry] -> L.ByteString
indexHtml t es = renderHtml $ index t es

index :: UTCTime -> [Entry] -> Markup
index t entries = html $ do
    head $ do
        title $ "arashi - all posts"
        meta ! charset "utf8"
        style $ toHtml (".icon { width: 16px; height: 16px }\n"
            ++ ".via { visibility: hidden }\n"
            ++ ".post:hover .via { visibility: visible }" :: String)
    body $ do
        section ! id "posts" $
            forM_ entries $ entry t

entry :: UTCTime -> Entry -> Markup
entry t (Entry url title_ v timestamp) = do
    article ! class_ "post" $ do
        img ! class_ "icon" ! src (stringValue $ "http://g.etfv.co/" ++ baseUrl)
        " "; linkTo url title_
        " "; time $ toHtml $ humanReadableTime' t timestamp
        " "; span ! class_ "via" $ do
            "(via "
            linkTo via via
            ")"
        "\n"
  where via = fromMaybe url v
        baseUrl = fromMaybe url $ do
            u <- parseURI url
            host <- uriRegName u
            scheme <- uriScheme u
            return $ scheme ++ "://" ++ host

linkTo :: String -> String -> Markup
linkTo url title_ =
    a ! href (stringValue url) $ toHtml title_
