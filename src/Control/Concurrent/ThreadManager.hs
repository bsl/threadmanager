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
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryTakeMVar,readMVar)
import Control.Exception       (SomeException, try)
import Control.Monad           (join, replicateM, when)
import qualified Data.Map as M

data ThreadStatus =
    Running
  | Finished
  | Threw SomeException
  deriving Show

newtype ThreadManager = TM (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving Eq

-- | Make a thread manager.
make :: IO ThreadManager
make = TM `fmap` newMVar M.empty

-- | Make a managed thread. Uses 'forkIO'.
fork :: ThreadManager -> IO () -> IO ThreadId
fork (TM tm) action =
    modifyMVar tm $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            r <- try action
            putMVar state (either Threw (const Finished) r)
        return (M.insert tid state m, tid)

-- | Make the given number of managed threads.
forkn :: ThreadManager -> Int -> IO () -> IO [ThreadId]
forkn tm n = replicateM n . fork tm

-- | Get the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (TM tm) tid =
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
waitFor (TM tm) tid =
    join . modifyMVar tm $ \m ->
      return $
        case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
          (Nothing, _)     -> (m, return Nothing)
          (Just state, m') -> (m', Just `fmap` takeMVar state)

-- | Block until all managed threads terminate.
waitForAll :: ThreadManager -> IO ()
waitForAll tm@(TM tmMvar) = do
    threadMap <- readMVar tmMvar
    let threads = M.keys threadMap
    statuses <- mapM (getStatus tm) threads
    _ <- mapM (waitFor tm) threads
    Control.Monad.when (foldr checkStatus False statuses) $
        waitForAll tm
  where
    checkStatus :: Maybe ThreadStatus -> Bool -> Bool
    checkStatus _ True = True
    checkStatus (Just Running) False = True
    checkStatus _ False = False
