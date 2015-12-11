module Node.ChildProcess
  ( Handle()
  , ChildProcess()
  , CHILD_PROCESS()
  , stderr
  , stdout
  , stdin
  , pid
  , connected
  , kill
  , send
  , disconnect
  , ChildProcessError()
  , onExit
  , onClose
  , onDisconnect
  , onMessage
  , onError
  , spawn
  , SpawnOptions()
  , defaultSpawnOptions
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.StrMap (StrMap())
import Data.Function (Fn2(), runFn2)
import Data.Nullable (Nullable(), toNullable)
import Data.Maybe (Maybe(..))
import Data.Foreign (Foreign())

import Node.Buffer (Buffer())
import Node.Stream (Readable(), Writable())
import Node.ChildProcess.Signal (Signal(..))

-- | A handle for inter-process communication (IPC).
foreign import data Handle :: *

-- | The effect for creating and interacting with child processes.
foreign import data CHILD_PROCESS :: !

newtype ChildProcess = ChildProcess ChildProcessRec

runChildProcess :: ChildProcess -> ChildProcessRec
runChildProcess (ChildProcess r) = r

-- | Note: some of these types are lies, and so it is unsafe to access some of
-- | these record fields directly.
type ChildProcessRec =
  { stderr     :: forall eff. Readable () (cp :: CHILD_PROCESS | eff) Buffer
  , stdin      :: forall eff. Writable () (cp :: CHILD_PROCESS | eff) Buffer
  , stdout     :: forall eff. Readable () (cp :: CHILD_PROCESS | eff) Buffer
  , pid        :: Int
  , connected  :: Boolean
  , kill       :: Signal -> Boolean
  , send       :: forall r. Fn2 { | r} Handle Boolean
  , disconnect :: forall eff. Eff eff Unit
  }

-- | The standard error stream of a child process. Note that this is only
-- | available if the process was spawned with the stderr option set to "pipe".
stderr :: forall eff. ChildProcess -> Readable () (cp :: CHILD_PROCESS | eff) Buffer
stderr = _.stderr <<< runChildProcess

-- | The standard output stream of a child process. Note that this is only
-- | available if the process was spawned with the stdout option set to "pipe".
stdout :: forall eff. ChildProcess -> Readable () (cp :: CHILD_PROCESS | eff) Buffer
stdout = _.stdout <<< runChildProcess

-- | The standard input stream of a child process. Note that this is only
-- | available if the process was spawned with the stdin option set to "pipe".
stdin :: forall eff. ChildProcess -> Writable () (cp :: CHILD_PROCESS | eff) Buffer
stdin = _.stdin <<< runChildProcess

-- | The process ID of a child process. Note that if the process has already
-- | exited, another process may have taken the same ID, so be careful!
pid :: ChildProcess -> Int
pid = _.pid <<< runChildProcess

connected :: forall eff. ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
connected = pure <<< _.connected <<< runChildProcess

send :: forall eff props. { | props } -> Handle -> ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
send msg handle (ChildProcess cp) = pure (runFn2 cp.send msg handle)

disconnect :: forall eff. ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Unit
disconnect = _.disconnect <<< runChildProcess

-- | Send a signal to a child process. It's an unfortunate historical decision
-- | that this function is called "kill", as sending a signal to a child
-- | process won't necessarily kill it.
kill :: forall eff. Signal -> ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
kill sig (ChildProcess cp) = pure (cp.kill sig)

type SpawnOptions =
  { cwd       :: String
  , stdio     :: Array String
  , env       :: Nullable (StrMap String)
  , detached  :: Boolean
  , uid       :: Int
  , gid       :: Int
  }

onExit :: forall eff. ChildProcess -> (Maybe Int -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
onExit = mkOnExit Nothing Just Signal

onClose :: forall eff. ChildProcess -> (Maybe Int -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
onClose = mkOnClose Nothing Just Signal

onMessage :: forall eff.  ChildProcess -> (Foreign -> Maybe Handle -> Eff eff Unit) -> Eff eff Unit
onMessage = mkOnMessage Nothing Just

foreign import onDisconnect :: forall eff. ChildProcess -> Eff eff Unit -> Eff eff Unit
foreign import onError :: forall eff. ChildProcess -> (ChildProcessError -> Eff eff Unit) -> Eff eff Unit

foreign import spawn :: forall eff. String -> Array String -> SpawnOptions -> Eff (cp :: CHILD_PROCESS | eff) ChildProcess

defaultSpawnOptions :: SpawnOptions
defaultSpawnOptions =
  { cwd: undefined
  , stdio: ["pipe", "pipe", "pipe"]
  , env: toNullable Nothing
  , detached: false
  , uid: undefined
  , gid: undefined
  }

type ChildProcessError = 
  { code :: String
  , errno :: String
  , syscall :: String
  }

foreign import mkOnExit :: forall a eff.
          Maybe a -> (a -> Maybe a) -> (String -> Signal) ->
          ChildProcess -> (Maybe Int -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
foreign import mkOnMessage :: forall a eff. 
          Maybe a -> (a -> Maybe a) -> 
          ChildProcess -> (Foreign -> Maybe Handle -> Eff eff Unit) -> Eff eff Unit
foreign import mkOnClose :: forall a eff.
          Maybe a -> (a -> Maybe a) -> (String -> Signal) ->
          ChildProcess -> (Maybe Int -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit

-- There's gotta be a better way.
foreign import undefined :: forall a. a
