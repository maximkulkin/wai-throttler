{-# LANGUAGE OverloadedStrings #-}

{- |
Wai middleware for request throttling.

Basic idea: on every (matching) request a counter is incremented.
If it exceeds given limit, request is blocked and error response
is sent to client. Request counter resets after defined period of
time.

The `throttle' function limits request to the underlying
application. If you wish to limit only parts of your requests
you need to do the routing yourself. For convenience,
`throttlePath' function is provided which applies throttling
only for requests with matching URL path.
-}
module Network.Wai.Middleware.Throttler
  ( ThrottleCache(..)
  , newMemoryThrottleCache
  , throttle
  , throttlePath
  ) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, readMVar)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.ByteString as B (ByteString, take, length)
import Data.ByteString.Char8 as B8 (pack)
import Data.Time.Clock (getCurrentTime, UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.Wai (Request, Response, Middleware, rawPathInfo, responseLBS)
import Network.HTTP.Types.Status (status403)

-- | Cache type class. Throttle cache is used to store request counts.
-- Can store multiple counts via different keys. E.g. keys can be client
-- IP addresses or user logins.
class ThrottleCache cache where
  -- | Increment count for given key and return new count value.
  -- Cache should automatically reset counts to zero after a defined period.
  cacheCount :: cache       -- ^ cache
             -> ByteString  -- ^ key
             -> IO Int


data MemoryThrottleCache = MemoryThrottleCache Int NominalDiffTime (MVar (Map ByteString (UTCTime, Int)))

-- | Create in-memory throttle cache.
--
-- Normally throttle cache does not need to know what the limit
-- is. But this one uses some trickery to prevent unnecessary
-- calls to slow getCurrentTime function.
newMemoryThrottleCache :: Int                    -- ^ limit
                       -> NominalDiffTime        -- ^ limit renew period
                       -> IO MemoryThrottleCache
newMemoryThrottleCache limit period = fmap (MemoryThrottleCache limit period) $ newMVar Map.empty

instance ThrottleCache MemoryThrottleCache where
  cacheCount (MemoryThrottleCache limit period v) key = do
    map <- takeMVar v
    (t', c') <- case Map.lookup key map of
                  Nothing -> do
                    now <- getCurrentTime
                    return $ (alignTime now period, 1)
                  Just (t, c) -> do
                    case c + 1 > limit of
                      True -> do
                        now <- getCurrentTime
                        let alignedNow = alignTime now period
                        case alignedNow == t of
                          True -> return $ (t, c + 1)
                          False -> return $ (alignedNow, 1)
                      False -> return $ (t, c + 1)
    putMVar v $ Map.insert key (t', c') map
    return c'

    where alignTime :: UTCTime -> NominalDiffTime -> UTCTime
          alignTime time period = posixSecondsToUTCTime . fromIntegral $ a - (a `mod` b)
            where a = floor . utcTimeToPOSIXSeconds $ time
                  b = floor period


-- | Apply throttling to requests with matching URL path
throttlePath :: ThrottleCache cache
             => ByteString                    -- ^ URL path to match
             -> cache                         -- ^ cache to store request counts
             -> Int                           -- ^ request limit
             -> (Request -> Maybe ByteString) -- ^ function to get cache key based on request. If Nothing is returned, request is not throttled
             -> Middleware
throttlePath path cache limit getKey app req = do
  case pathMatches path req of
    False -> app req
    True -> throttle cache limit getKey app req

  where pathMatches :: ByteString -> Request -> Bool
        pathMatches path request = (rawPathInfo request) == path

-- | Wai middleware that cuts requests if request rate is higher than defined level.
-- Responds with 403 if limit exceeded
throttle :: ThrottleCache cache
         => cache                         -- ^ cache to store request counts
         -> Int                           -- ^ request limit
         -> (Request -> Maybe ByteString) -- ^ function to get cache key based on request. If Nothing is returned, request is not throttled
         -> Middleware
throttle cache limit getKey app req = do
  case getKey req of
    Nothing -> app req
    Just key -> do
      count <- cacheCount cache key
      if count > limit then return throttledResponse else app req

  where throttledResponse = responseLBS status403 [] ""
