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
-- | forms the basis for this module and has in-depth documentation about
-- | runtime behaviour.
-- |
-- | Functions like `spawn`, `exec`, and `fork` use Node defaults
-- | except for the following cases:
-- | - `encoding` is always set to `"buffer"`. This ensures other functions have the correct type.
module Node.ChildProcess
  ( Handle
  , ChildProcess
  , stdin
  , stdout
  , stderr
  , stdio
  , onSpawn
  , Error
  , toStandardError
  , onError
  , Exit(..)
  , onExit
  , onClose
  , onDisconnect
  , onMessage
  , Shell(..)
  , SerializationOption(..)
  , spawn
  , spawn'
  , SpawnOptions
  , SpawnSyncResult
  , spawnSync
  , spawnSync'
  , SpawnSyncOptions
  , exec
  , exec'
  , ExecOptions
  , execFile
  , execFile'
  , ExecFileOptions
  , execSync
  , execSync'
  , ExecSyncOptions
  , execFileSync
  , execFileSync'
  , ExecFileSyncOptions
  , fork
  , fork'
  , ForkOptions
  , spawnFile
  , spawnArgs
  , pid
  , exitCode
  , signalCode
  , connected
  , killed
  , kill
  , kill'
  , channel
  , ref
  , unref
  , pidExists
  , send
  , disconnect
  ) where

import Prelude

import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Posix (Pid)
import Data.Posix.Signal (Signal(..))
import Data.Posix.Signal as Signal
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.AbortController (AbortSignal)
import Effect.Exception as Exception
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Foreign (Foreign)
import Foreign.Object (Object)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.ChildProcess.StdIO (StdIoOption, toStdIoOption, useIpc)
import Node.Stream (Readable, Writable)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Boolean (False, True)
import Unsafe.Coerce (unsafeCoerce)

-- | A handle for inter-process communication (IPC).
foreign import data Handle :: Type

-- | Opaque type returned by `spawn`, `fork`, and `exec`.
-- | Needed as input for most methods in this module.
-- |
-- | Type parameters are better understood via
-- | ```
-- | ChildProcess stdin stdout stderr hasIpc
-- | ```
-- | The types track whether `stdin`/`stdout`/`stderr` exist on the ChildProcess instance
-- | based on the `stdio` option given and whether they are readable, writeable, or duplex streams.
-- |
-- | The Boolean type tracks whether "ipc" was enabled in the `stdio` option.
foreign import data ChildProcess :: Type -> Type -> Type -> Boolean -> Type

channel
  :: forall stdIn stdOut stdErr
   . ChildProcess stdIn stdOut stdErr True
  -> Effect (Maybe { ref :: Effect Unit, unref :: Effect Unit })
channel cp = toMaybe <$> runEffectFn1 channelImpl cp

foreign import channelImpl :: forall stdIn stdOut stdErr. EffectFn1 (ChildProcess stdIn stdOut stdErr True) (Nullable { ref :: Effect Unit, unref :: Effect Unit })

-- | Indicates whether it is still possible to send and receive
-- | messages from the child process.
connected
  :: forall stdIn stdOut stdErr
   . ChildProcess stdIn stdOut stdErr True
  -> Effect Boolean
connected cp = runEffectFn1 connectedImpl cp

foreign import connectedImpl :: forall stdIn stdOut stdErr. EffectFn1 (ChildProcess stdIn stdOut stdErr True) Boolean

-- | Closes the IPC channel between parent and child.
disconnect :: forall stdIn stdOut stdErr. ChildProcess stdIn stdOut stdErr True -> Effect Unit
disconnect cp = runEffectFn1 disconnectImpl cp

foreign import disconnectImpl :: forall stdIn stdOut stdErr. EffectFn1 (ChildProcess stdIn stdOut stdErr True) Unit

exitCode :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect (Maybe Int)
exitCode cp = toMaybe <$> runEffectFn1 exitCodeImpl cp

foreign import exitCodeImpl :: forall stdIn stdOut stdErr ipc. EffectFn1 (ChildProcess stdIn stdOut stdErr ipc) (Nullable Int)

-- | Same as `kill' SIGTERM`
kill :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect Boolean
kill = kill' SIGTERM

-- | Send a signal to a child process. In the same way as the
-- | [unix kill(2) system call](https://linux.die.net/man/2/kill),
-- | sending a signal to a child process won't necessarily kill it.
-- |
-- | The resulting effects of this function depend on the process
-- | and the signal. They can vary from system to system.
-- | The child process might emit an `"error"` event if the signal
-- | could not be delivered.
kill' :: forall stdIn stdOut stdErr ipc. Signal -> ChildProcess stdIn stdOut stdErr ipc -> Effect Boolean
kill' sig cp = runEffectFn2 killImpl (Signal.toString sig) cp

foreign import killImpl :: forall stdIn stdOut stdErr ipc. EffectFn2 String (ChildProcess stdIn stdOut stdErr ipc) Boolean

pidExists :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect Boolean
pidExists cp = runEffectFn1 pidExistsImpl cp

foreign import pidExistsImpl :: forall stdIn stdOut stdErr ipc. EffectFn1 (ChildProcess stdIn stdOut stdErr ipc) Boolean

killed :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect Boolean
killed cp = runEffectFn1 killedImpl cp

foreign import killedImpl :: forall stdIn stdOut stdErr ipc. EffectFn1 (ChildProcess stdIn stdOut stdErr ipc) Boolean

-- | The process ID of a child process. Note that if the process has already
-- | exited, another process may have taken the same ID, so be careful!
pid :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect (Maybe Pid)
pid cp = toMaybe <$> runEffectFn1 pidImpl cp

foreign import pidImpl :: forall stdIn stdOut stdErr ipc. EffectFn1 (ChildProcess stdIn stdOut stdErr ipc) (Nullable Pid)

-- Note: add `detached` type parameter to ChildProcess
ref :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect Unit
ref cp = runEffectFn1 refImpl cp

foreign import refImpl :: forall stdIn stdOut stdErr ipc. EffectFn1 (ChildProcess stdIn stdOut stdErr ipc) Unit

unref :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect Unit
unref cp = runEffectFn1 unrefImpl cp

foreign import unrefImpl :: forall stdIn stdOut stdErr ipc. EffectFn1 (ChildProcess stdIn stdOut stdErr ipc) Unit

type SendOptions =
  { keepOpen :: Maybe Boolean
  }

type JsSendOptions =
  { keepOpen :: Boolean
  }

-- | Send messages to the (`nodejs`) child process.
-- |
-- | See the [node documentation](https://nodejs.org/api/child_process.html#child_process_subprocess_send_message_sendhandle_options_callback)
-- | for in-depth documentation.
send
  :: forall stdIn stdOut stdErr
   . ChildProcess stdIn stdOut stdErr True
  -> Foreign
  -> Handle
  -> (SendOptions -> SendOptions)
  -> Effect Unit
  -> Effect Boolean
send cp msg handle buildOptions cb = runEffectFn5 sendImpl cp msg handle jsOptions cb
  where
  options = buildOptions { keepOpen: Nothing }
  jsOptions = { keepOpen: fromMaybe undefined options.keepOpen }

foreign import sendImpl :: forall stdIn stdOut stdErr. EffectFn5 (ChildProcess stdIn stdOut stdErr True) (Foreign) (Handle) (JsSendOptions) (Effect Unit) (Boolean)

signalCode
  :: forall stdIn stdOut stdErr ipc
   . ChildProcess stdIn stdOut stdErr ipc
  -> Effect (Maybe { signalStr :: String, signal :: Maybe Signal })
signalCode cp = map enhance $ runEffectFn1 signalCodeImpl cp
  where
  enhance = toMaybe >>> map (\s -> { signalStr: s, signal: Signal.fromString s })

foreign import signalCodeImpl :: forall stdIn stdOut stdErr ipc. EffectFn1 (ChildProcess stdIn stdOut stdErr ipc) (Nullable String)

foreign import spawnArgs :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Array String

foreign import spawnFile :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> String

-- | The standard input stream of a child process.
foreign import stdin
  :: forall stdIn stdOut stdErr ipc
   . ChildProcess stdIn stdOut stdErr ipc
  -> stdIn

stdio :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect (Array Foreign)
stdio cp = runEffectFn1 stdioImpl cp

foreign import stdioImpl :: forall stdIn stdOut stdErr ipc. EffectFn1 (ChildProcess stdIn stdOut stdErr ipc) (Array Foreign)

-- | The standard output stream of a child process.
foreign import stdout
  :: forall stdIn stdOut stdErr ipc
   . ChildProcess stdIn stdOut stdErr ipc
  -> stdOut

-- | The standard error stream of a child process.
foreign import stderr
  :: forall stdIn stdOut stdErr ipc
   . ChildProcess stdIn stdOut stdErr ipc
  -> stdErr

-- | Handle the `"close"` signal.
onClose :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> (Maybe Int -> Maybe String -> Effect Unit) -> Effect Unit
onClose cp cb = runEffectFn2 onCloseImpl cp $ mkEffectFn2 \a b -> cb (toMaybe a) (toMaybe b)

foreign import onCloseImpl :: forall stdIn stdOut stdErr ipc. EffectFn2 (ChildProcess stdIn stdOut stdErr ipc) (EffectFn2 (Nullable Int) (Nullable String) Unit) (Unit)

-- | Handle the `"disconnect"` signal.
onDisconnect :: forall stdIn stdOut stdErr. ChildProcess stdIn stdOut stdErr True -> Effect Unit -> Effect Unit
onDisconnect cp cb = runEffectFn2 onDisconnectImpl cp cb

foreign import onDisconnectImpl :: forall stdIn stdOut stdErr. EffectFn2 (ChildProcess stdIn stdOut stdErr True) (Effect Unit) (Unit)

-- | Handle the `"error"` signal.
onError :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> (Error -> Effect Unit) -> Effect Unit
onError cp cb = runEffectFn2 onErrorImpl cp $ mkEffectFn1 cb

foreign import onErrorImpl :: forall stdIn stdOut stdErr ipc. EffectFn2 (ChildProcess stdIn stdOut stdErr ipc) (EffectFn1 Error Unit) (Unit)

-- | Specifies how a child process exited; normally (with an exit code), or
-- | terminated by the given signal.
data Exit
  = ExitCode Int
  | SignalCode String (Maybe Signal)

derive instance Eq Exit
derive instance Generic Exit _
instance showExit :: Show Exit where
  show x = genericShow x

-- | Handle the `"exit"` signal.
onExit :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> (Exit -> Effect Unit) -> Effect Unit
onExit cp cb = runEffectFn2 onExitImpl cp $ mkEffectFn2 \e s ->
  cb case toMaybe e, toMaybe s of
    Just i, _ -> ExitCode i
    _, Just sig -> SignalCode sig $ Signal.fromString sig
    _, _ -> unsafeCrashWith "Impossible: either exit code or signal code must be non-null"

foreign import onExitImpl :: forall stdIn stdOut stdErr ipc. EffectFn2 (ChildProcess stdIn stdOut stdErr ipc) (EffectFn2 (Nullable Int) (Nullable String) Unit) (Unit)

-- | Handle the `"message"` signal.
onMessage :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> (Foreign -> Maybe Handle -> Effect Unit) -> Effect Unit
onMessage cp cb = runEffectFn2 onMessageImpl cp $ mkEffectFn2 \a b -> cb a (toMaybe b)

foreign import onMessageImpl :: forall stdIn stdOut stdErr ipc. EffectFn2 (ChildProcess stdIn stdOut stdErr ipc) (EffectFn2 Foreign (Nullable Handle) Unit) (Unit)

onSpawn :: forall stdIn stdOut stdErr ipc. ChildProcess stdIn stdOut stdErr ipc -> Effect Unit -> Effect Unit
onSpawn cp cb = runEffectFn2 onSpawnImpl cp cb

foreign import onSpawnImpl :: forall stdIn stdOut stdErr ipc. EffectFn2 (ChildProcess stdIn stdOut stdErr ipc) (Effect Unit) Unit

type ExecOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , shell :: Maybe String
  , signal :: Maybe AbortSignal
  , timeout :: Maybe Int
  , maxBuffer :: Maybe Int
  , killSignal :: Maybe (Either String Int)
  , uid :: Maybe Int
  , gid :: Maybe Int
  , windowsHide :: Maybe Boolean
  }

type JsExecOptions =
  { cwd :: String
  , env :: Object String
  , encoding :: String
  , shell :: String
  , signal :: AbortSignal
  , timeout :: Int
  , maxBuffer :: Int
  , killSignal :: KillSignal
  , uid :: Int
  , gid :: Int
  , windowsHide :: Boolean
  }

-- | either Int or String
foreign import data KillSignal :: Type

exec
  :: String
  -> ( { error :: Maybe Exception.Error
       , stdout :: ImmutableBuffer
       , stderr :: ImmutableBuffer
       }
       -> Effect Unit
     )
  -> Effect (ChildProcess Void Void Void False)
exec cmd = exec' cmd identity

-- | Similar to `spawn`, except that this variant will:
-- | * run the given command with the shell,
-- | * buffer output, and wait until the process has exited before calling the
-- |   callback.
-- |
-- | Note that the child process will be killed if the amount of output exceeds
-- | a certain threshold (the default is defined by Node.js).
exec'
  :: String
  -> (ExecOptions -> ExecOptions)
  -> ( { error :: Maybe Exception.Error
       , stdout :: ImmutableBuffer
       , stderr :: ImmutableBuffer
       }
       -> Effect Unit
     )
  -> Effect (ChildProcess Void Void Void False)
exec' cmd buildOptions cb = runEffectFn3 execImpl cmd jsOptions $ mkEffectFn3 \err stdOUT stdERR ->
  cb { error: toMaybe err, stdout: stdOUT, stderr: stdERR }
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , env: fromMaybe undefined options.env
    , encoding: "buffer" -- force stdout/stderr in callback to be Buffers
    , shell: fromMaybe undefined options.shell
    , signal: fromMaybe undefined options.signal
    , timeout: fromMaybe undefined options.timeout
    , maxBuffer: fromMaybe undefined options.maxBuffer
    , killSignal: fromMaybe undefined $ map (either (unsafeCoerce :: String -> KillSignal) (unsafeCoerce :: Int -> KillSignal)) options.killSignal
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , windowsHide: fromMaybe undefined options.windowsHide
    }

  defaults :: ExecOptions
  defaults =
    { cwd: Nothing
    , env: Nothing
    , shell: Nothing
    , signal: Nothing
    , timeout: Nothing
    , maxBuffer: Nothing
    , killSignal: Nothing
    , uid: Nothing
    , gid: Nothing
    , windowsHide: Nothing
    }

foreign import execImpl
  :: EffectFn3 String JsExecOptions (EffectFn3 (Nullable Exception.Error) ImmutableBuffer ImmutableBuffer Unit) (ChildProcess Void Void Void False)

data Shell
  = DefaultShell
  | CustomShell String

derive instance Eq Shell
derive instance Generic Shell _
instance Show Shell where
  show x = genericShow x

-- | Boolean or String
foreign import data ActualShellOption :: Type

type ExecFileOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , shell :: Maybe Shell
  , signal :: Maybe AbortSignal
  , timeout :: Maybe Int
  , maxBuffer :: Maybe Int
  , killSignal :: Maybe (Either String Int)
  , uid :: Maybe Int
  , gid :: Maybe Int
  , windowsHide :: Maybe Boolean
  , windowsVerbatimArguments :: Maybe Boolean
  }

type JsExecFileOptions =
  { cwd :: String
  , env :: Object String
  , encoding :: String
  , timeout :: Int
  , maxBuffer :: Int
  , killSignal :: KillSignal
  , uid :: Int
  , gid :: Int
  , windowsHide :: Boolean
  , windowsVerbatimArguments :: Boolean
  , shell :: ActualShellOption
  , signal :: AbortSignal
  }

execFile
  :: String
  -> Array String
  -> ( { error :: Maybe Exception.Error
       , stdout :: ImmutableBuffer
       , stderr :: ImmutableBuffer
       }
       -> Effect Unit
     )
  -> Effect (ChildProcess Void Void Void False)
execFile file args = execFile' file args identity

-- | Like `exec`, except instead of using a shell, it passes the arguments
-- | directly to the specified command.
execFile'
  :: String
  -> Array String
  -> (ExecFileOptions -> ExecFileOptions)
  -> ( { error :: Maybe Exception.Error
       , stdout :: ImmutableBuffer
       , stderr :: ImmutableBuffer
       }
       -> Effect Unit
     )
  -> Effect (ChildProcess Void Void Void False)
execFile' file args buildOptions cb = runEffectFn4 execFileImpl file args jsOptions $ mkEffectFn3 \err stdOUT stdERR ->
  cb { error: toMaybe err, stdout: stdOUT, stderr: stdERR }
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , env: fromMaybe undefined options.env
    , encoding: "buffer" -- force stdout/stderr in callback to be Buffers
    , timeout: fromMaybe undefined options.timeout
    , maxBuffer: fromMaybe undefined options.maxBuffer
    , killSignal: fromMaybe undefined $ map (either (unsafeCoerce :: String -> KillSignal) (unsafeCoerce :: Int -> KillSignal)) options.killSignal
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , windowsHide: fromMaybe undefined options.windowsHide
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    , shell: case options.shell of
        Nothing -> undefined
        Just DefaultShell -> (unsafeCoerce :: Boolean -> ActualShellOption) true
        Just (CustomShell shell) -> (unsafeCoerce :: String -> ActualShellOption) shell
    , signal: fromMaybe undefined options.signal
    }

  defaults :: ExecFileOptions
  defaults =
    { cwd: Nothing
    , env: Nothing
    , shell: Nothing
    , signal: Nothing
    , timeout: Nothing
    , maxBuffer: Nothing
    , killSignal: Nothing
    , uid: Nothing
    , gid: Nothing
    , windowsHide: Nothing
    , windowsVerbatimArguments: Nothing
    }

foreign import execFileImpl
  :: forall stdIn stdOut stdErr ipc
   . EffectFn4 (String) (Array String) (JsExecFileOptions) (EffectFn3 (Nullable Exception.Error) ImmutableBuffer ImmutableBuffer Unit) (ChildProcess stdIn stdOut stdErr ipc)

type ExecSyncOptions stdin stdout stderr ipc =
  { cwd :: Maybe String
  , input :: Maybe ImmutableBuffer
  , stdio :: Maybe (StdIoOption stdin stdout stderr ipc)
  , env :: Maybe (Object String)
  , shell :: Maybe String
  , uid :: Maybe Int
  , gid :: Maybe Int
  , timeout :: Maybe Int
  , killSignal :: Maybe (Either String Int)
  , maxBuffer :: Maybe Int
  , windowsHide :: Maybe Boolean
  }

type JsExecSyncOptions stdin stdout stderr ipc =
  { cwd :: String
  , input :: ImmutableBuffer
  , stdio :: StdIoOption stdin stdout stderr ipc
  , env :: Object String
  , shell :: String
  , uid :: Int
  , gid :: Int
  , timeout :: Int
  , killSignal :: KillSignal
  , maxBuffer :: Int
  , encoding :: String
  , windowsHide :: Boolean
  }

execSync :: String -> Effect ImmutableBuffer
execSync cmd = execSync' cmd identity

-- | Generally identical to `exec`, with the exception that
-- | the method will not return until the child process has fully closed.
-- | Returns: The stdout from the command.
execSync'
  :: forall stdin stdout stderr ipc
   . String
  -> ( ExecSyncOptions Void Void (Readable ()) False
       -> ExecSyncOptions stdin stdout stderr ipc
     )
  -> Effect ImmutableBuffer
execSync' cmd buildOptions = runEffectFn2 execSyncImpl cmd jsOptions
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , input: fromMaybe undefined options.input
    , stdio: fromMaybe undefined options.stdio
    , env: fromMaybe undefined options.env
    , encoding: "buffer" -- force stdout/stderr in callback to be Buffers
    , shell: fromMaybe undefined options.shell
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , timeout: fromMaybe undefined options.timeout
    , killSignal: fromMaybe undefined $ map (either (unsafeCoerce :: String -> KillSignal) (unsafeCoerce :: Int -> KillSignal)) options.killSignal
    , maxBuffer: fromMaybe undefined options.maxBuffer
    , windowsHide: fromMaybe undefined options.windowsHide
    }

  defaults :: ExecSyncOptions Void Void (Readable ()) False
  defaults =
    { cwd: Nothing
    , input: Nothing
    , stdio: Nothing
    , env: Nothing
    , shell: Nothing
    , uid: Nothing
    , gid: Nothing
    , timeout: Nothing
    , killSignal: Nothing
    , maxBuffer: Nothing
    , windowsHide: Nothing
    }

foreign import execSyncImpl
  :: forall stdin stdout stderr ipc
   . EffectFn2 String (JsExecSyncOptions stdin stdout stderr ipc) ImmutableBuffer

type ExecFileSyncOptions stdin stdout stderr ipc =
  { cwd :: Maybe String
  , input :: Maybe ImmutableBuffer
  , stdio :: Maybe (StdIoOption stdin stdout stderr ipc)
  , env :: Maybe (Object String)
  , uid :: Maybe Int
  , gid :: Maybe Int
  , timeout :: Maybe Int
  , killSignal :: Maybe (Either String Int)
  , maxBuffer :: Maybe Int
  , windowsHide :: Maybe Boolean
  , shell :: Maybe Shell
  }

type JsExecFileSyncOptions stdin stdout stderr ipc =
  { cwd :: String
  , input :: ImmutableBuffer
  , stdio :: StdIoOption stdin stdout stderr ipc
  , env :: Object String
  , uid :: Int
  , gid :: Int
  , timeout :: Int
  , killSignal :: KillSignal
  , maxBuffer :: Int
  , encoding :: String
  , windowsHide :: Boolean
  , shell :: ActualShellOption
  }

execFileSync :: String -> Array String -> Effect ImmutableBuffer
execFileSync file args = execFileSync' file args identity

-- | Generally identical to `execFile`, with the exception that
-- | the method will not return until the child process has fully closed.
-- | Returns: The stdout from the command.
execFileSync'
  :: forall stdin stdout stderr ipc
   . String
  -> Array String
  -> ( ExecFileSyncOptions Void Void (Writable ()) False
       -> ExecFileSyncOptions stdin stdout stderr ipc
     )
  -> Effect ImmutableBuffer
execFileSync' file args buildOptions = runEffectFn3 execFileSyncImpl file args jsOptions
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , input: fromMaybe undefined options.input
    , stdio: fromMaybe undefined options.stdio
    , env: fromMaybe undefined options.env
    , encoding: "buffer" -- force stdout/stderr in callback to be Buffers
    , shell: case options.shell of
        Nothing -> undefined
        Just DefaultShell -> (unsafeCoerce :: Boolean -> ActualShellOption) true
        Just (CustomShell shell) -> (unsafeCoerce :: String -> ActualShellOption) shell
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , timeout: fromMaybe undefined options.timeout
    , killSignal: fromMaybe undefined $ map (either (unsafeCoerce :: String -> KillSignal) (unsafeCoerce :: Int -> KillSignal)) options.killSignal
    , maxBuffer: fromMaybe undefined options.maxBuffer
    , windowsHide: fromMaybe undefined options.windowsHide
    }

  defaults :: ExecFileSyncOptions Void Void (Writable ()) False
  defaults =
    { cwd: Nothing
    , input: Nothing
    , stdio: Nothing
    , env: Nothing
    , uid: Nothing
    , gid: Nothing
    , timeout: Nothing
    , killSignal: Nothing
    , maxBuffer: Nothing
    , windowsHide: Nothing
    , shell: Nothing
    }

foreign import execFileSyncImpl
  :: forall stdin stdout stderr ipc
   . EffectFn3 String (Array String) (JsExecFileSyncOptions stdin stdout stderr ipc) ImmutableBuffer

data SerializationOption
  = SerializeJson
  | SerializeAdvanced

derive instance Eq SerializationOption
derive instance Generic SerializationOption _
instance Show SerializationOption where
  show x = genericShow x

toJsSerialization :: SerializationOption -> String
toJsSerialization = case _ of
  SerializeJson -> "json"
  SerializeAdvanced -> "advanced"

type SpawnOptions stdIn stdOut stdErr ipc =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , argv0 :: Maybe String
  , stdio :: Maybe (StdIoOption stdIn stdOut stdErr ipc)
  , detached :: Maybe Boolean
  , uid :: Maybe Int
  , gid :: Maybe Int
  , serialization :: Maybe SerializationOption
  , shell :: Maybe Shell
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  , signal :: Maybe AbortSignal
  , timeout :: Maybe Int
  , killSignal :: Maybe (Either String Int)
  }

type JsSpawnOptions stdIn stdOut stdErr ipc =
  { cwd :: String
  , env :: Object String
  , argv0 :: String
  , stdio :: StdIoOption stdIn stdOut stdErr ipc
  , detached :: Boolean
  , uid :: Int
  , gid :: Int
  , serialization :: String
  , shell :: ActualShellOption
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  , signal :: AbortSignal
  , timeout :: Int
  , killSignal :: KillSignal
  }

spawn :: String -> Array String -> Effect (ChildProcess (Writable ()) (Readable ()) (Readable ()) False)
spawn file args = spawn' file args identity

-- | Spawn a child process. Note that, in the event that a child process could
-- | not be spawned (for example, if the executable was not found) this will
-- | not throw an error. Instead, the `ChildProcess` will be created anyway,
-- | but it will immediately emit an 'error' event.
spawn'
  :: forall stdIn stdOut stdErr ipc
   . String
  -> Array String
  -> ( SpawnOptions (Writable ()) (Readable ()) (Readable ()) False
       -> SpawnOptions stdIn stdOut stdErr ipc
     )
  -> Effect (ChildProcess stdIn stdOut stdErr ipc)
spawn' file args buildOptions = runEffectFn3 spawnImpl file args jsOptions
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , env: fromMaybe undefined options.env
    , argv0: fromMaybe undefined options.argv0
    , detached: fromMaybe undefined options.detached
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , serialization: maybe undefined toJsSerialization options.serialization
    , stdio: fromMaybe undefined options.stdio
    , shell: case options.shell of
        Nothing -> undefined
        Just DefaultShell -> (unsafeCoerce :: Boolean -> ActualShellOption) true
        Just (CustomShell shell) -> (unsafeCoerce :: String -> ActualShellOption) shell
    , signal: fromMaybe undefined options.signal
    , timeout: fromMaybe undefined options.timeout
    , killSignal: fromMaybe undefined $ map (either (unsafeCoerce :: String -> KillSignal) (unsafeCoerce :: Int -> KillSignal)) options.killSignal
    , windowsHide: fromMaybe undefined options.windowsHide
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    }

  defaults =
    { cwd: Nothing
    , env: Nothing
    , argv0: Nothing
    , stdio: Nothing
    , detached: Nothing
    , uid: Nothing
    , gid: Nothing
    , serialization: Nothing
    , shell: Nothing
    , windowsVerbatimArguments: Nothing
    , windowsHide: Nothing
    , signal: Nothing
    , timeout: Nothing
    , killSignal: Nothing
    }

foreign import spawnImpl
  :: forall stdIn stdOut stdErr ipc
   . EffectFn3
       String
       (Array String)
       (JsSpawnOptions stdIn stdOut stdErr ipc)
       (ChildProcess stdIn stdOut stdErr ipc)

type SpawnSyncOptions stdIn stdOut stdErr ipc =
  { cwd :: Maybe String
  , input :: Maybe ImmutableBuffer
  , argv0 :: Maybe String
  , stdio :: Maybe (StdIoOption stdIn stdOut stdErr ipc)
  , env :: Maybe (Object String)
  , uid :: Maybe Int
  , gid :: Maybe Int
  , timeout :: Maybe Int
  , killSignal :: Maybe (Either String Int)
  , maxBuffer :: Maybe Int
  , shell :: Maybe Shell
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  }

type JsSpawnSyncOptions stdIn stdOut stdErr ipc =
  { cwd :: String
  , input :: ImmutableBuffer
  , argv0 :: String
  , stdio :: StdIoOption stdIn stdOut stdErr ipc
  , env :: Object String
  , uid :: Int
  , gid :: Int
  , timeout :: Int
  , killSignal :: KillSignal
  , maxBuffer :: Int
  , encoding :: String
  , shell :: ActualShellOption
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  }

type SpawnSyncResult =
  { pid :: Pid
  , output :: Array Foreign
  , stdout :: ImmutableBuffer
  , stderr :: ImmutableBuffer
  , status :: Maybe Int
  , signal :: Maybe String
  , error :: Maybe Error
  }

type JsSpawnSyncResult =
  { pid :: Pid
  , output :: Array Foreign
  , stdout :: ImmutableBuffer
  , stderr :: ImmutableBuffer
  , status :: Nullable Int
  , signal :: Nullable String
  , error :: Nullable Error
  }

spawnSync :: String -> Array String -> Effect SpawnSyncResult
spawnSync file args = spawnSync' file args identity

spawnSync'
  :: forall stdIn stdOut stdErr ipc
   . String
  -> Array String
  -> ( SpawnSyncOptions (Writable ()) (Readable ()) (Readable ()) False
       -> SpawnSyncOptions stdIn stdOut stdErr ipc
     )
  -> Effect SpawnSyncResult
spawnSync' file args buildOptions = do
  jsResult <- runEffectFn3 spawnSyncImpl file args jsOptions
  pure
    { pid: jsResult.pid
    , output: jsResult.output
    , stdout: jsResult.stdout
    , stderr: jsResult.stderr
    , status: toMaybe jsResult.status
    , signal: toMaybe jsResult.signal
    , error: toMaybe jsResult.error
    }
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , input: fromMaybe undefined options.input
    , argv0: fromMaybe undefined options.argv0
    , stdio: fromMaybe undefined options.stdio
    , env: fromMaybe undefined options.env
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , timeout: fromMaybe undefined options.timeout
    , killSignal: fromMaybe undefined $ map (either (unsafeCoerce :: String -> KillSignal) (unsafeCoerce :: Int -> KillSignal)) options.killSignal
    , maxBuffer: fromMaybe undefined options.maxBuffer
    , encoding: "buffer" -- force stdout/stderr in callback to be Buffers
    , shell: case options.shell of
        Nothing -> undefined
        Just DefaultShell -> (unsafeCoerce :: Boolean -> ActualShellOption) true
        Just (CustomShell shell) -> (unsafeCoerce :: String -> ActualShellOption) shell
    , windowsHide: fromMaybe undefined options.windowsHide
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    }

  defaults :: SpawnSyncOptions (Writable ()) (Readable ()) (Readable ()) False
  defaults =
    { cwd: Nothing
    , input: Nothing
    , argv0: Nothing
    , stdio: Nothing
    , env: Nothing
    , uid: Nothing
    , gid: Nothing
    , timeout: Nothing
    , killSignal: Nothing
    , maxBuffer: Nothing
    , shell: Nothing
    , windowsVerbatimArguments: Nothing
    , windowsHide: Nothing
    }

foreign import spawnSyncImpl
  :: forall stdIn stdOut stdErr ipc
   . EffectFn3 String (Array String) (JsSpawnSyncOptions stdIn stdOut stdErr ipc) JsSpawnSyncResult

type ForkOptions stdIn stdOut stdErr ipc =
  { cwd :: Maybe String
  , detached :: Maybe Boolean
  , env :: Maybe (Object String)
  , execPath :: Maybe String
  , execArgv :: Maybe (Array String)
  , gid :: Maybe Int
  , serialization :: Maybe SerializationOption
  , signal :: Maybe AbortSignal
  , killSignal :: Maybe (Either String Int)
  , stdio :: Maybe (StdIoOption stdIn stdOut stdErr ipc)
  , uid :: Maybe Int
  , windowsVerbatimArguments :: Maybe Boolean
  , timeout :: Maybe Int
  }

type JsForkOptions stdIn stdOut stdErr ipc =
  { cwd :: String
  , detached :: Boolean
  , env :: Object String
  , execPath :: String
  , execArgv :: Array String
  , gid :: Int
  , serialization :: String
  , signal :: AbortSignal
  , killSignal :: KillSignal
  -- silent is ignored due to stdio
  , stdio :: StdIoOption stdIn stdOut stdErr ipc
  , uid :: Int
  , windowsVerbatimArguments :: Boolean
  , timeout :: Int
  }

fork
  :: String
  -> Array String
  -> Effect (ChildProcess (Writable ()) (Readable ()) (Readable ()) True)
fork modPath args = fork' modPath args identity

-- | A special case of `spawn` for creating Node.js child processes. The first
-- | argument is the module to be run, and the second is the argv (command line
-- | arguments).
fork'
  :: forall stdIn stdOut stdErr
   . String
  -> Array String
  -> ( ForkOptions (Writable ()) (Readable ()) (Readable ()) True
       -> ForkOptions stdIn stdOut stdErr True
     )
  -> Effect (ChildProcess stdIn stdOut stdErr True)
fork' modulePath args buildOptions = runEffectFn3 forkImpl modulePath args jsOptions
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , detached: fromMaybe undefined options.detached
    , env: fromMaybe undefined options.env
    , execPath: fromMaybe undefined options.execPath
    , execArgv: fromMaybe undefined options.execArgv
    , gid: fromMaybe undefined options.gid
    , serialization: maybe undefined toJsSerialization options.serialization
    , signal: fromMaybe undefined options.signal
    , killSignal: fromMaybe undefined $ map (either (unsafeCoerce :: String -> KillSignal) (unsafeCoerce :: Int -> KillSignal)) options.killSignal
    , stdio: fromMaybe undefined options.stdio
    , uid: fromMaybe undefined options.uid
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    , timeout: fromMaybe undefined options.timeout
    }

  defaults :: ForkOptions (Writable ()) (Readable ()) (Readable ()) True
  defaults =
    { cwd: Nothing
    , detached: Nothing
    , env: Nothing
    , execPath: Nothing
    , execArgv: Nothing
    , gid: Nothing
    , serialization: Nothing
    , signal: Nothing
    , killSignal: Nothing
    , stdio: Just (toStdIoOption (_ { ipc = Just $ const useIpc }))
    , uid: Nothing
    , windowsVerbatimArguments: Nothing
    , timeout: Nothing
    }

foreign import forkImpl :: forall stdIn stdOut stdErr. EffectFn3 String (Array String) (JsForkOptions stdIn stdOut stdErr True) (ChildProcess stdIn stdOut stdErr True)

foreign import undefined :: forall a. a

-- | An error which occurred inside a child process.
type Error =
  { code :: String
  , errno :: String
  , syscall :: String
  }

-- | Convert a ChildProcess.Error to a standard Error, which can then be thrown
-- | inside an Effect or Aff computation (for example).
toStandardError :: Error -> Exception.Error
toStandardError = unsafeCoerce
