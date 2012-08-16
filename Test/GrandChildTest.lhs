What happens if I create a tree of threads like:

ThreadMain
 ChildThread
  GrandChildThread

And ChildThread ends?  Normally GrandChildThread dies.  This module tests whether ThreadManager successfully flattens this thread hierarchy to:

ThreadMain
 ChildThread
 GrandChildThread

thus saving GrandChildThread in the case of ChildThread's death.

>import qualified Control.Concurrent.ThreadManager as TM
>import Control.Concurrent

>main :: IO ()
>main = do
> tm <- TM.make
> TM.fork tm (childThread tm)
> TM.waitForAll tm
> print "If you see this message, your grandChild has died and you should morn like the poorest of poppers."


>childThread :: TM.ThreadManager -> IO ()
>childThread tm = do
> TM.fork tm (grandChildThread 0)

> return ()

>grandChildThread :: Int ->  IO ()
>grandChildThread count = 
> print count >> threadDelay 1000 >> (grandChildThread $ count+1)

