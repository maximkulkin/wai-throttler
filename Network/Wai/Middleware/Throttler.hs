module Network.Wai.Middleware.Throttler where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, readMVar)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Data.Time.Clock (getCurrentTime, UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.Wai (Request, Middleware)

class RequestCache c where
  cacheCount :: c -> ByteString -> IO Int


data MemoryCache = MemoryCache Int NominalDiffTime (MVar (Map ByteString (UTCTime, Int)))

newMemoryCache :: Int -> NominalDiffTime -> IO MemoryCache
newMemoryCache limit period = fmap (MemoryCache limit period) $ newMVar Map.empty

instance RequestCache MemoryCache where
  cacheCount (MemoryCache limit period v) key = do
    map <- takeMVar v
    (t', c') <- case Map.lookup key map of
                  Nothing -> do
                    now <- getCurrentTime
                    return $ (alignTimeUnit now period, 1)
                  Just (t, c) -> do
                    case c + 1 > limit of
                      True -> do
                        now <- getCurrentTime
                        let alignedNow = alignTimeUnit now period
                        case alignedNow == t of
                          True -> return $ (t, c + 1)
                          False -> return $ (alignedNow, 1)
                      False -> return $ (t, c + 1)
    putMVar v $ Map.insert key (t', c') map
    return c'

    where alignTimeUnit :: UTCTime -> NominalDiffTime -> UTCTime
          alignTimeUnit value alignment = posixSecondsToUTCTime . fromIntegral $ a - (a `mod` b)
            where a = floor . utcTimeToPOSIXSeconds $ value
                  b = floor alignment

throttle :: RequestCache c => c -> ByteString -> Int -> (Request -> Maybe ByteString) -> Middleware
throttle cache path limit getKey app req = undefined
