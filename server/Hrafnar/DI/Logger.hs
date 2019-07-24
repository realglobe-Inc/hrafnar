{- |
Description : Specification of logger.
Module      : Hrafnar.DI.Logger
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  :  REALGLOBE INC.

This module provides thread-safe, fast and multicast logger.
-}
module Hrafnar.DI.Logger
  (
    -- * Basics
    LogRelay(..)
  , broadcastLog
  , stdoutLogger
  , stdoutLoggerWith
  , newLogQueue
  , newLogChan
  , LogQueue
  , LogChan
  -- * Formatters
  , Formatter
  , defaultFormatter
  -- * Log levels
  , LogLevel(..)
  , log
  , debug
  , info
  , notice
  , warn
  , err
  , critical
  , alert
  , emergency
  -- * For @Data.Reflection@
  , UseLogger
  , useLogger
  -- * Useful string class and operator
  , LogStr(..)
  , ($:)
  -- * Re-export
  , give
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString         as SBS
import qualified Data.ByteString.Builder as BB
import           Data.Reflection
import           Data.String             (IsString (..))
import qualified Data.Text               as ST
import qualified Data.Text.Encoding      as STE
import           Data.UnixTime           (UnixTime (..), formatUnixTime, getUnixTime)
import           GHC.IO.Unsafe           (unsafePerformIO)
import           Prelude                 hiding (log)
import           System.IO               (hFlush, stdout)


-- | Types which are able to be converted into @Builder@
class IsString a => LogStr a where
  toLogStr :: a -> BB.Builder

instance LogStr ST.Text where
  toLogStr = BB.byteString . STE.encodeUtf8

instance LogStr String where
  toLogStr = BB.stringUtf8

instance LogStr BB.Builder where
  toLogStr = id

instance LogStr SBS.ByteString where
  toLogStr = BB.byteString

-- | Operator which fix overloaded strings to @Builder@.
($:) :: (BB.Builder -> b) -> BB.Builder -> b
($:) = ($)

-- | Logging level. This is matched to syslog.
data LogLevel
  = LogDebug
  | LogInfo
  | LogNotice
  | LogWarn
  | LogError
  | LogCritical
  | LogAlert
  | LogEmergency
  deriving (Show, Eq, Ord)

logLevelToBuilder :: LogLevel -> BB.Builder
logLevelToBuilder = \case
  LogDebug -> "DEBUG"
  LogInfo  -> "INFO"
  LogNotice  -> "NOTICE"
  LogWarn  -> "WARN"
  LogError -> "ERROR"
  LogCritical -> "CRITICAL"
  LogAlert -> "ALERT"
  LogEmergency -> "EMERGEBCY"

-- | Logging message.
data LogMsg = forall s. LogStr s => LogMsg LogLevel UnixTime s

-- | Queue of @LogMsg@.
newtype LogQueue = LogQueue (TQueue LogMsg)

-- | Channel of @LogMsg@.
newtype LogChan = LogChan (TChan LogMsg)

-- | Shorthand for Data.Reflection
type UseLogger = Given LogQueue

-- | Shorthand for Data.Reflection
useLogger :: UseLogger => LogQueue
useLogger = given

-- | Make new @LogQueue@
newLogQueue :: IO LogQueue
newLogQueue = LogQueue <$> newTQueueIO

-- | Make new @LogChan@.
newLogChan :: IO LogChan
newLogChan = LogChan <$> newTChanIO

-- | Connect @LogQueue@ and @TChan@ @LogMsg@.
broadcastLog :: LogQueue -> LogChan -> IO ()
broadcastLog (LogQueue q) (LogChan c) = forever $
  atomically $ readTQueue q >>= writeTChan c

-- | Receive @LogMsg@ from @LogQueue@, then broadcast it through @TChan@
class LogRelay a where
  relayLog :: a -> LogChan -> LogLevel -> IO ()


newtype StdoutLogRelay = StdoutLogRelay Formatter

stdoutLoggerWith :: Formatter -> StdoutLogRelay
stdoutLoggerWith = StdoutLogRelay

stdoutLogger :: StdoutLogRelay
stdoutLogger = StdoutLogRelay defaultFormatter

instance LogRelay StdoutLogRelay where
  relayLog (StdoutLogRelay formatter)  (LogChan broadcastChan) logLevel = do
    chan <- atomically $ dupTChan broadcastChan
    forever $ do
      LogMsg msgLevel ut str <- atomically $ readTChan chan
      when (msgLevel >= logLevel) $ do
        BB.hPutBuilder stdout $ formatter msgLevel ut str
        hFlush stdout

type Formatter = forall s. LogStr s => LogLevel -> UnixTime -> s -> BB.Builder

-- | Default log formatter.
defaultFormatter :: LogStr s => LogLevel -> UnixTime -> s -> BB.Builder
defaultFormatter lev ut str = formatTime ut <> " - [" <> logLevelToBuilder lev <> "] " <> toLogStr str <> "\n"

-- | Format @UnixTime@ to ISO 8601 format expression.
{-# NOINLINE formatTime #-}
formatTime :: UnixTime -> BB.Builder
formatTime ut =
  let
    ut' = BB.byteString . unsafePerformIO $ formatUnixTime "%Y-%m-$d %T" ut
    utMilli = BB.string7 . tail . show $ utMicroSeconds ut `div` 1000 + 1000
  in
    ut' <> "." <> utMilli

log :: (MonadIO m, LogStr s) => LogQueue -> LogLevel -> s -> m ()
log (LogQueue q) l str = liftIO $ do
  ut <- getUnixTime
  atomically $ writeTQueue q (LogMsg l ut str)

debug :: (MonadIO m, LogStr s) => LogQueue -> s -> m ()
debug q = log q LogDebug

info :: (MonadIO m, LogStr s) => LogQueue -> s -> m ()
info q = log q LogInfo

notice :: (MonadIO m, LogStr s) => LogQueue -> s -> m ()
notice q = log q LogNotice

warn :: (MonadIO m, LogStr s) => LogQueue -> s -> m ()
warn q = log q LogWarn

err :: (MonadIO m, LogStr s) => LogQueue -> s -> m ()
err q = log q LogError

critical :: (MonadIO m, LogStr s) => LogQueue -> s -> m ()
critical q = log q LogCritical

alert :: (MonadIO m, LogStr s) => LogQueue -> s -> m ()
alert q = log q LogAlert

emergency :: (MonadIO m, LogStr s) => LogQueue -> s -> m ()
emergency q = log q LogEmergency
