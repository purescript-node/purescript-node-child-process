-- | This module contains various types and functions to allow you to spawn and
-- | interact with child processes.
-- |
-- | It is intended to be imported qualified, as follows:
-- |
-- | ```purescript
-- | import Node.ChildProcess (ChildProcess, CHILD_PROCESS)
-- | import Node.ChildProcess as ChildProcess
-- | ```
-- |
-- | The [Node.js documentation](https://nodejs.org/api/child_process.html)
-- | will probably also be useful to read if you want to use this module.
module Node.ChildProcess
  ( Handle
  , ChildProcess
  , CHILD_PROCESS
  , stderr
  , stdout
  , stdin
  , pid
  , connected
  , kill
  , send
  , disconnect
  , Error
  , toStandardError
  , Exit(..)
  , onExit
  , onClose
  , onDisconnect
  , onMessage
  , onError
  , spawn
  , SpawnOptions
  , defaultSpawnOptions
  , exec
  , execFile
  , ExecOptions
  , ExecResult
  , defaultExecOptions
  , fork
  , StdIOBehaviour(..)
  , pipe
  , inherit
  , ignore
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toNullable, toMaybe)
import Data.Posix (Pid, Gid, Uid)
import Data.Posix.Signal (Signal)
import Data.Posix.Signal as Signal
import Data.StrMap (StrMap)

import Node.Buffer (Buffer)
import Node.FS as FS
import Node.Stream (Readable, Writable, Stream)

import Unsafe.Coerce (unsafeCoerce)

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
  { stdin :: forall eff. Nullable (Writable () (cp :: CHILD_PROCESS | eff))
  , stdout :: forall eff. Nullable (Readable () (cp :: CHILD_PROCESS | eff))
  , stderr :: forall eff. Nullable (Readable () (cp :: CHILD_PROCESS | eff))
  , pid :: Pid
  , connected :: Boolean
  , kill :: String -> Boolean
  , send :: forall r. Fn2 { | r} Handle Boolean
  , disconnect :: forall eff. Eff eff Unit
  }

-- | The standard input stream of a child process. Note that this is only
-- | available if the process was spawned with the stdin option set to "pipe".
stdin :: forall eff. ChildProcess -> Writable () (cp :: CHILD_PROCESS | eff)
stdin = unsafeFromNullable (missingStream "stdin") <<< _.stdin <<< runChildProcess

-- | The standard output stream of a child process. Note that this is only
-- | available if the process was spawned with the stdout option set to "pipe".
stdout :: forall eff. ChildProcess -> Readable () (cp :: CHILD_PROCESS | eff)
stdout = unsafeFromNullable (missingStream "stdout") <<< _.stdout <<< runChildProcess

-- | The standard error stream of a child process. Note that this is only
-- | available if the process was spawned with the stderr option set to "pipe".
stderr :: forall eff. ChildProcess -> Readable () (cp :: CHILD_PROCESS | eff)
stderr = unsafeFromNullable (missingStream "stderr") <<< _.stderr <<< runChildProcess

missingStream :: String -> String
missingStream str =
  "Node.ChildProcess: stream not available: " <> str <> "\nThis is probably "
  <> "because you passed something other than Pipe to the stdio option when "
  <> "you spawned it."

foreign import unsafeFromNullable :: forall a. String -> Nullable a -> a

-- | The process ID of a child process. Note that if the process has already
-- | exited, another process may have taken the same ID, so be careful!
pid :: ChildProcess -> Pid
pid = _.pid <<< runChildProcess

connected :: forall eff. ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
connected (ChildProcess cp) = mkEff \_ -> cp.connected

send :: forall eff props. { | props } -> Handle -> ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
send msg handle (ChildProcess cp) = mkEff \_ -> runFn2 cp.send msg handle

disconnect :: forall eff. ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Unit
disconnect = _.disconnect <<< runChildProcess

-- | Send a signal to a child process. It's an unfortunate historical decision
-- | that this function is called "kill", as sending a signal to a child
-- | process won't necessarily kill it.
kill :: forall eff. Signal -> ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
kill sig (ChildProcess cp) = mkEff \_ -> cp.kill (Signal.toString sig)

mkEff :: forall eff a. (Unit -> a) -> Eff eff a
mkEff = unsafeCoerce

-- | Specifies how a child process exited; normally (with an exit code), or
-- | due to a signal.
data Exit
  = Normally Int
  | BySignal Signal

instance showExit :: Show Exit where
  show (Normally x) = "Normally " <> show x
  show (BySignal sig) = "BySignal " <> show sig

mkExit :: Nullable Int -> Nullable String -> Exit
mkExit code signal =
  case fromCode code <|> fromSignal signal of
    Just e -> e
    Nothing -> unsafeThrow "Node.ChildProcess.mkExit: Invalid arguments"
  where
  fromCode = toMaybe >>> map Normally
  fromSignal = toMaybe >=> Signal.fromString >>> map BySignal

onExit :: forall eff. ChildProcess -> (Exit -> Eff eff Unit) -> Eff eff Unit
onExit = mkOnExit mkExit

foreign import mkOnExit
  :: forall eff
   . (Nullable Int -> Nullable String -> Exit)
  -> ChildProcess
  -> (Exit -> Eff eff Unit)
  -> Eff eff Unit

onClose :: forall eff. ChildProcess -> (Exit -> Eff eff Unit) -> Eff eff Unit
onClose = mkOnClose mkExit

foreign import mkOnClose
  :: forall eff
   . (Nullable Int -> Nullable String -> Exit)
  -> ChildProcess
  -> (Exit -> Eff eff Unit)
  -> Eff eff Unit

onMessage :: forall eff. ChildProcess -> (Foreign -> Maybe Handle -> Eff eff Unit) -> Eff eff Unit
onMessage = mkOnMessage Nothing Just

foreign import mkOnMessage
  :: forall a eff
   . Maybe a
  -> (a -> Maybe a)
  -> ChildProcess
  -> (Foreign -> Maybe Handle -> Eff eff Unit)
  -> Eff eff Unit

foreign import onDisconnect :: forall eff. ChildProcess -> Eff eff Unit -> Eff eff Unit
foreign import onError :: forall eff. ChildProcess -> (Error -> Eff eff Unit) -> Eff eff Unit

-- | Spawn a child process. Note that, in the event that a child process could
-- | not be spawned (for example, if the executable was not found) this will
-- | not throw an error. Instead, the `ChildProcess` will be created anyway,
-- | but it will immediately emit an 'error' event.
spawn :: forall eff. String -> Array String -> SpawnOptions -> Eff (cp :: CHILD_PROCESS | eff) ChildProcess
spawn cmd args opts = spawnImpl cmd args (convertOpts opts)
  where
  convertOpts opts =
    { cwd: fromMaybe undefined opts.cwd
    , stdio: toActualStdIOOptions opts.stdio
    , env: toNullable opts.env
    , detached: opts.detached
    , uid: fromMaybe undefined opts.uid
    , gid: fromMaybe undefined opts.gid
    }

foreign import spawnImpl :: forall opts eff. String -> Array String -> { | opts } -> Eff (cp :: CHILD_PROCESS | eff) ChildProcess

-- There's gotta be a better way.
foreign import undefined :: forall a. a

type SpawnOptions =
  { cwd :: Maybe String
  , stdio :: Array (Maybe StdIOBehaviour)
  , env :: Maybe (StrMap String)
  , detached :: Boolean
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  }

defaultSpawnOptions :: SpawnOptions
defaultSpawnOptions =
  { cwd: Nothing
  , stdio: pipe
  , env: Nothing
  , detached: false
  , uid: Nothing
  , gid: Nothing
  }

-- | Similar to `spawn`, except that this variant will:
-- | * run the given command with the shell,
-- | * buffer output, and wait until the process has exited before calling the
-- |   callback.
-- |
-- | Note that the child process will be killed if the amount of output exceeds
-- | a certain threshold (the default is defined by Node.js).
exec
  :: forall eff
   . String
  -> ExecOptions
  -> (ExecResult -> Eff (cp :: CHILD_PROCESS | eff) Unit)
  -> Eff (cp :: CHILD_PROCESS | eff) Unit
exec cmd opts callback =
  execImpl cmd (convertExecOptions opts) \err stdout' stderr' ->
    callback
      { error: toMaybe err
      , stdout: stdout'
      , stderr: stderr'
      }

foreign import execImpl
  :: forall eff
   . String
  -> ActualExecOptions
  -> (Nullable Exception.Error -> Buffer -> Buffer -> Eff (cp :: CHILD_PROCESS | eff) Unit)
  -> Eff (cp :: CHILD_PROCESS | eff) Unit

-- | Like `exec`, except instead of using a shell, it passes the arguments
-- | directly to the specified command.
execFile
  :: forall eff
   .  String
  -> Array String
  -> ExecOptions
  -> (ExecResult -> Eff (cp :: CHILD_PROCESS | eff) Unit)
  -> Eff (cp :: CHILD_PROCESS | eff) Unit
execFile cmd args opts callback =
  execFileImpl cmd args (convertExecOptions opts) \err stdout' stderr' ->
    callback
      { error: toMaybe err
      , stdout: stdout'
      , stderr: stderr'
      }

foreign import execFileImpl
  :: forall eff
   . String
  -> Array String
  -> ActualExecOptions
  -> (Nullable Exception.Error -> Buffer -> Buffer -> Eff (cp :: CHILD_PROCESS | eff) Unit)
  -> Eff (cp :: CHILD_PROCESS | eff) Unit

foreign import data ActualExecOptions :: *

convertExecOptions :: ExecOptions -> ActualExecOptions
convertExecOptions opts = unsafeCoerce
  { cwd: fromMaybe undefined opts.cwd
  , env: fromMaybe undefined opts.env
  , timeout: fromMaybe undefined opts.timeout
  , maxBuffer: fromMaybe undefined opts.maxBuffer
  , killSignal: fromMaybe undefined opts.killSignal
  , uid: fromMaybe undefined opts.uid
  , gid: fromMaybe undefined opts.gid
  }

type ExecOptions =
  { cwd :: Maybe String
  , env :: Maybe (StrMap String)
  , timeout :: Maybe Number
  , maxBuffer :: Maybe Int
  , killSignal :: Maybe Signal
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  }

defaultExecOptions :: ExecOptions
defaultExecOptions =
  { cwd: Nothing
  , env: Nothing
  , timeout: Nothing
  , maxBuffer: Nothing
  , killSignal: Nothing
  , uid: Nothing
  , gid: Nothing
  }

type ExecResult =
  { stderr :: Buffer
  , stdout :: Buffer
  , error :: Maybe Exception.Error
  }

-- | A special case of `spawn` for creating Node.js child processes. The first
-- | argument is the module to be run, and the second is the argv (command line
-- | arguments).
foreign import fork :: forall eff. String -> Array String -> Eff (cp :: CHILD_PROCESS | eff) ChildProcess

-- | An error which occurred inside a child process.
type Error =
  { code :: String
  , errno :: String
  , syscall :: String
  }

-- | Convert a ChildProcess.Error to a standard Error, which can then be thrown
-- | inside an Eff or Aff computation (for example).
toStandardError :: Error -> Exception.Error
toStandardError = unsafeCoerce

-- | Behaviour for standard IO streams (eg, standard input, standard output) of
-- | a child process.
-- |
-- | * `Pipe`: creates a pipe between the child and parent process, which can
-- |   then be accessed as a `Stream` via the `stdin`, `stdout`, or `stderr`
-- |   functions.
-- | * `Ignore`: ignore this stream. This will cause Node to open /dev/null and
-- |   connect it to the stream.
-- | * `ShareStream`: Connect the supplied stream to the corresponding file
-- |    descriptor in the child.
-- | * `ShareFD`: Connect the supplied file descriptor (which should be open
-- |   in the parent) to the corresponding file descriptor in the child.
data StdIOBehaviour
  = Pipe
  | Ignore
  | ShareStream (forall r eff. Stream r eff)
  | ShareFD FS.FileDescriptor

-- | Create pipes for each of the three standard IO streams.
pipe :: Array (Maybe StdIOBehaviour)
pipe = map Just [Pipe, Pipe, Pipe]

-- | Share stdin with stdin, stdout with stdout, and stderr with stderr.
inherit :: Array (Maybe StdIOBehaviour)
inherit = map Just
  [ ShareStream process.stdin
  , ShareStream process.stdout
  , ShareStream process.stderr
  ]

foreign import process :: forall props. { | props }

-- | Ignore all streams.
ignore :: Array (Maybe StdIOBehaviour)
ignore = map Just [Ignore, Ignore, Ignore]

foreign import data ActualStdIOBehaviour :: *

toActualStdIOBehaviour :: StdIOBehaviour -> ActualStdIOBehaviour
toActualStdIOBehaviour b = case b of
  Pipe -> c "pipe"
  Ignore -> c "ignore"
  ShareFD x -> c x
  ShareStream stream -> c stream
  where
  c :: forall a. a -> ActualStdIOBehaviour
  c = unsafeCoerce

type ActualStdIOOptions = Array (Nullable ActualStdIOBehaviour)

toActualStdIOOptions :: Array (Maybe StdIOBehaviour) -> ActualStdIOOptions
toActualStdIOOptions = map (toNullable <<< map toActualStdIOBehaviour)
