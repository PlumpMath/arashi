module Arashi.Util where

import Network.HTTP (getRequest, rspBody)
import Network.Browser (browse, request, setAllowRedirects, setOutHandler)

getHttp :: String -> IO String
getHttp url = browse $ do
    setAllowRedirects True
    setOutHandler $ \_ -> return ()
    fmap (rspBody . snd) $ request $ getRequest url
