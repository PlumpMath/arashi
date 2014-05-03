{-# LANGUAGE OverloadedStrings #-}
module Arashi.Config where

import Control.Applicative ((<$>))

import Data.EDN
import Data.EDN.Types.Class (parseEDNv)

data Config = Config [String] deriving (Show)

instance FromEDN Config where
    parseEDNv (Map m) = do
        sources <- m .: Keyword "sources"
        case sources of
            Map sm -> Config <$> sm .: Keyword "feed"
            _ -> fail "error"
