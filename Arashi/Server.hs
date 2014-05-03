{-# LANGUAGE OverloadedStrings #-}
module Arashi.Server where

import Data.Set (Set, toList)
import Data.IORef
import Data.Time (getCurrentTime)
import Data.List (sortBy)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mempty)
import Data.ByteString (ByteString)
import Data.String.Conversions (convertString)
import Control.Monad (liftM)
import Text.Read (readMaybe)

import Snap.Core
import Snap.Http.Server

import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Arashi.Core
import Arashi.Render

htmlResponse :: Html -> Snap ()
htmlResponse html = do
    modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
    writeLBS $ renderHtml html

readParam :: (Read a, MonadSnap s) => s (Maybe ByteString) -> s (Maybe a)
readParam = liftM (>>= readMaybe . convertString)

readParam' :: (Read a, MonadSnap s) => a -> s (Maybe ByteString) -> s a
readParam' x = liftM (maybe x id) . readParam

fromEntries :: Set Entry -> [Entry]
fromEntries = sortBy (flipOrdering compareByUrlAndTime) . toList

app :: IORef (Set Entry) -> Snap ()
app entriesRef = do
    t <- liftIO getCurrentTime
    es <- liftIO $ readIORef entriesRef
    count <- readParam' 100 $ getQueryParam "count"
    start <- readParam' 0 $ getQueryParam "start"
    htmlResponse $ index t $ take count $ drop start $ fromEntries es

runServer :: Int -> IORef (Set Entry) -> IO ()
runServer port = httpServe (setPort port mempty) . app
