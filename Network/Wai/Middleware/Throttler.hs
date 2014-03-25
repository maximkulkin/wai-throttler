{-# LANGUAGE OverloadedStrings #-}

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

class ThrottleCache c where
  cacheCount :: c -> ByteString -> IO Int


data MemoryThrottleCache = MemoryThrottleCache Int NominalDiffTime (MVar (Map ByteString (UTCTime, Int)))

newMemoryThrottleCache :: Int -> NominalDiffTime -> IO MemoryThrottleCache
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


throttlePath :: ThrottleCache c => ByteString -> c -> Int -> (Request -> Maybe ByteString) -> Middleware
throttlePath path cache limit getKey app req = do
  case pathMatches path req of
    False -> app req
    True -> throttle cache limit getKey app req

  where pathMatches :: ByteString -> Request -> Bool
        pathMatches path request = B.take (B.length path) (rawPathInfo request) == path

throttle :: ThrottleCache c => c -> Int -> (Request -> Maybe ByteString) -> Middleware
throttle cache limit getKey app req = do
  case getKey req of
    Nothing -> app req
    Just key -> do
      count <- cacheCount cache key
      if count > limit then return (throttledResponse count limit) else app req

  where throttledResponse :: Int -> Int -> Response
        throttledResponse count limit =
          responseLBS status403
                      [ ("X-RateLimit-Limit", bs limit)
                      , ("X-RateLimit-Remaining", bs remaining)
                      ]
                      ""
          where bs = B8.pack . show
                remaining = max 0 (limit - count)
