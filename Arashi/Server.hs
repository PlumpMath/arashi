{-# LANGUAGE OverloadedStrings #-}
module Arashi.Server where

import Data.Set (Set, toList)
import Data.IORef
import Data.Time (getCurrentTime)
import Data.List (sortBy)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mempty)

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

app :: IORef (Set Entry) -> Snap ()
app entriesRef = do
    t <- liftIO getCurrentTime
    es <- liftIO $ readIORef entriesRef
    let sortedEntries = sortBy (flipOrdering compareByUrlAndTime) $ toList es
    htmlResponse $ index t sortedEntries

runServer :: Int -> IORef (Set Entry) -> IO ()
runServer port = httpServe (setPort port mempty) . app
