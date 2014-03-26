{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8 (pack, split, dropWhile)
import Data.Maybe (fromMaybe)
import Network.Wai (Request, remoteHost, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Throttler (newMemoryThrottleCache, throttle, throttlePath)
import Network.HTTP.Types.Status (status200)
import Network.Socket (SockAddr(..))

requestRemoteAddress :: Request -> ByteString
requestRemoteAddress req = fromMaybe remoteAddress $ fmap (last . B8.split ',') $ forwardedForHeader
  where remoteAddress = sockAddress . remoteHost $ req
        forwardedForHeader = lookup "X-Forwarded-For" . requestHeaders $ req

        sockAddress :: SockAddr -> ByteString
        sockAddress (SockAddrInet _ a)      = B8.pack . show $ a
        sockAddress (SockAddrInet6 _ _ a _) = B8.pack . show $ a
        sockAddress (SockAddrUnix a)        = B8.pack . show $ a


main = do
  putStrLn $ "Starting server on port 3001"
  throttleCache <- newMemoryThrottleCache 50 60
  throttleCacheFoo <- newMemoryThrottleCache 5 10
  throttleCacheBar <- newMemoryThrottleCache 3 60
  run 3001 $
    throttle throttleCache 50 (Just . requestRemoteAddress) $
    throttlePath "/foo" throttleCacheFoo 5 (Just . requestRemoteAddress) $
    throttlePath "/bar" throttleCacheBar 3 (Just . requestRemoteAddress) $
    \_ -> return $ responseLBS status200 [] "Hello, world!"
