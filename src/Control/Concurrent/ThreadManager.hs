{-|
  A simple thread management API inspired by the one in chapter
  24 of /Real World Haskell/.

  See <http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html>.

  Intended to be imported qualified (suggestion: TM).
 -}

module Control.Concurrent.ThreadManager
  ( ThreadManager
  , ThreadStatus (..)
  , make
  , fork, forkn, getStatus, waitFor, waitForAll
  ) where

import Control.Concurrent      (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception       (SomeException, try)
import Control.Monad           (join, replicateM)
import qualified Data.Map as M

data ThreadStatus =
    Running
  | Finished
  | Threw SomeException
  deriving Show

data ThreadManager = TM (MVar (M.Map ThreadId (MVar ThreadStatus))) (MVar (IO ()))
  deriving Eq

-- | Make a thread manager. This should be run in a thread that won't die.
make :: IO ThreadManager
make = do
 print "hiii"
 listenerMVar <- newEmptyMVar
 mvarMaps <- newMVar M.empty
 tm <- return $ TM mvarMaps listenerMVar
 forkIO $ listen tm
 return tm

-- | This thread becomes the parent of all other threads.
listen :: ThreadManager -> IO ()
listen tm@(TM map listenerMVar) = do
 command <- takeMVar listenerMVar
 command
 listen tm

runCommand :: ThreadManager -> IO a -> IO a
runCommand tm@(TM _ listenerMVar) command = do
 resultMVar <- newEmptyMVar
 putMVar listenerMVar (command >>= putMVar resultMVar)
 takeMVar resultMVar

-- | Make a managed thread. Uses 'forkIO'.
fork :: ThreadManager -> IO () -> IO ThreadId
fork tm action = runCommand tm (fork' tm action)

fork' :: ThreadManager -> IO () -> IO ThreadId
fork' (TM tm _) action =
    modifyMVar tm $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            r <- try action
            putMVar state (either Threw (const Finished) r)
        return (M.insert tid state m, tid)

-- | Make the given number of managed threads.
forkn :: ThreadManager -> Int -> IO () -> IO [ThreadId]
forkn tm n action = runCommand tm (forkn' tm n action)

forkn' :: ThreadManager -> Int -> IO () -> IO [ThreadId]
forkn' tm n = replicateM n . fork tm

-- | Get the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus tm tid = runCommand tm $ getStatus' tm tid
getStatus' :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus' (TM tm _) tid =
    modifyMVar tm $ \m ->
      case M.lookup tid m of
        Nothing    -> return (m, Nothing)
        Just state -> tryTakeMVar state >>= \mst ->
          return $
            case mst of
              Nothing  -> (m, Just Running)
              Just sth -> (M.delete tid m, Just sth)

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor tm tid = runCommand tm $ waitFor' tm tid

waitFor' :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor' (TM tm _) tid =
    join . modifyMVar tm $ \m ->
      return $
        case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
          (Nothing, _)     -> (m, return Nothing)
          (Just state, m') -> (m', Just `fmap` takeMVar state)

-- | Block until all managed threads terminate.
waitForAll :: ThreadManager -> IO ()
waitForAll tm = runCommand tm $ waitForAll' tm
waitForAll' :: ThreadManager -> IO ()
waitForAll' (TM tm _) =
    modifyMVar tm elems >>= mapM_ takeMVar
  where
    elems m = return (M.empty, M.elems m)
