{-# LANGUAGE FlexibleInstances #-}
module Purity.Data where
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TQueue as TQ
import System.IO.Unsafe (unsafePerformIO)
import Data.Void
import Data.String

instance IsString (Purity ()) where
  fromString s = Purity $ \q -> STM.atomically (TQ.writeTQueue q s)

newtype Purity a = Purity {runPurity :: TQ.TQueue String -> IO ()}

-- | Log a string asynchronously
logStr :: String -> IO ()
logStr = STM.atomically . TQ.writeTQueue globalLogger

-- | Log a string asynchronously and put a newline on it
logStrLn :: String -> IO ()
logStrLn = logStr . (++"\n")

-- | Log a string asyncronously with a tag and a newline
logStrTag :: String -> String -> IO ()
logStrTag tag str = if elem tag disabledTags then pure () else logStrLn $ "[\x1b[34m" ++ tag ++ "\x1b[0m]" ++ str

disabledTags :: [String]
disabledTags = []--["Tick"]

logInfo :: String -> IO ()
logInfo = logStrTag "Info"

logTick :: String -> IO ()
logTick = logStrTag "Tick"

logDebug :: String -> IO ()
logDebug = logStrTag "Debug"

-- | Make a string green
green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"

-- | Make a string red
red :: String -> String
red s = "\x1b[31m" ++ s ++ "\x1b[0m"

-- | The global logging queue
globalLogger :: TQ.TQueue String
globalLogger = unsafePerformIO TQ.newTQueueIO

-- | A thread that logs things from the global logger forever
loggingThread :: IO Void
loggingThread = (STM.atomically (TQ.readTQueue globalLogger) >>= putStr) *> loggingThread
