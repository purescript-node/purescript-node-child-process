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
  , onExit
  , onClose
  , onDisconnect
  , onMessage
  , StdIOBehaviour(..)
  , stdIOBehavior
  , pipe
  , inherit
  , inheritOr
  , ignore
  , Shell(..)
  , SerializationOption(..)
  , spawn
  , spawn'
  , SpawnOptions
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
import Data.Nullable (Nullable, toMaybe, toNullable)
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
import Node.FS as FS
import Node.Stream (Readable, Writable, Stream)
import Unsafe.Coerce (unsafeCoerce)

-- | A handle for inter-process communication (IPC).
foreign import data Handle :: Type

-- | Opaque type returned by `spawn`, `fork`, and `exec`.
-- | Needed as input for most methods in this module.
foreign import data ChildProcess :: Type

channel :: ChildProcess -> Effect (Maybe { ref :: Effect Unit, unref :: Effect Unit })
channel cp = toMaybe <$> runEffectFn1 channelImpl cp

foreign import channelImpl :: EffectFn1 ChildProcess (Nullable { ref :: Effect Unit, unref :: Effect Unit })

-- | Indicates whether it is still possible to send and receive
-- | messages from the child process.
connected :: ChildProcess -> Effect Boolean
connected cp = runEffectFn1 connectedImpl cp

foreign import connectedImpl :: EffectFn1 ChildProcess Boolean

-- | Closes the IPC channel between parent and child.
disconnect :: ChildProcess -> Effect Unit
disconnect cp = runEffectFn1 disconnectImpl cp

foreign import disconnectImpl :: EffectFn1 ChildProcess Unit

exitCode :: ChildProcess -> Effect (Maybe Int)
exitCode cp = toMaybe <$> runEffectFn1 exitCodeImpl cp

foreign import exitCodeImpl :: EffectFn1 ChildProcess (Nullable Int)

-- | Same as `kill' SIGTERM`
kill :: ChildProcess -> Effect Boolean
kill = kill' SIGTERM

-- | Send a signal to a child process. In the same way as the
-- | [unix kill(2) system call](https://linux.die.net/man/2/kill),
-- | sending a signal to a child process won't necessarily kill it.
-- |
-- | The resulting effects of this function depend on the process
-- | and the signal. They can vary from system to system.
-- | The child process might emit an `"error"` event if the signal
-- | could not be delivered.
kill' :: Signal -> ChildProcess -> Effect Boolean
kill' sig cp = runEffectFn2 killImpl (Signal.toString sig) cp

foreign import killImpl :: EffectFn2 String ChildProcess Boolean

pidExists :: ChildProcess -> Effect Boolean
pidExists cp = runEffectFn1 pidExistsImpl cp

foreign import pidExistsImpl :: EffectFn1 ChildProcess Boolean

killed :: ChildProcess -> Effect Boolean
killed cp = runEffectFn1 killedImpl cp

foreign import killedImpl :: EffectFn1 ChildProcess Boolean

-- | The process ID of a child process. Note that if the process has already
-- | exited, another process may have taken the same ID, so be careful!
pid :: ChildProcess -> Effect (Maybe Pid)
pid cp = toMaybe <$> runEffectFn1 pidImpl cp

foreign import pidImpl :: EffectFn1 ChildProcess (Nullable Pid)

ref :: ChildProcess -> Effect Unit
ref cp = runEffectFn1 refImpl cp

foreign import refImpl :: EffectFn1 ChildProcess Unit

unref :: ChildProcess -> Effect Unit
unref cp = runEffectFn1 unrefImpl cp

foreign import unrefImpl :: EffectFn1 ChildProcess Unit

type SendOptions =
  { keepOpen :: Maybe Boolean
  }

type JsSendOptions =
  { keepOpen :: Boolean
  }

send :: ChildProcess -> Foreign -> Handle -> (SendOptions -> SendOptions) -> Effect Unit -> Effect Boolean
send cp msg handle buildOptions cb = runEffectFn5 sendImpl cp msg handle jsOptions cb
  where
  options = buildOptions { keepOpen: Nothing }
  jsOptions = { keepOpen: fromMaybe undefined options.keepOpen }

foreign import sendImpl :: EffectFn5 (ChildProcess) (Foreign) (Handle) (JsSendOptions) (Effect Unit) (Boolean)

signalCode :: ChildProcess -> Effect (Maybe { signalStr :: String, signal :: Maybe Signal })
signalCode cp = map enhance $ runEffectFn1 signalCodeImpl cp
  where
  enhance = toMaybe >>> map (\s -> { signalStr: s, signal: Signal.fromString s })

foreign import signalCodeImpl :: EffectFn1 (ChildProcess) (Nullable String)

foreign import spawnArgs :: ChildProcess -> Array String

foreign import spawnFile :: ChildProcess -> String

-- | The standard error stream of a child process. Note that this is only
-- | available if the process was spawned with the stderr option set to "pipe".
stderr :: ChildProcess -> Maybe (Readable ())
stderr = toMaybe <<< stderrImpl

foreign import stderrImpl :: ChildProcess -> Nullable (Readable ())

-- | The standard input stream of a child process. Note that this is only
-- | available if the process was spawned with the stdin option set to "pipe".
stdin :: ChildProcess -> Maybe (Writable ())
stdin = toMaybe <<< stdinImpl

foreign import stdinImpl :: ChildProcess -> Nullable (Writable ())

stdio :: ChildProcess -> Effect (Array Foreign)
stdio cp = runEffectFn1 stdioImpl cp

foreign import stdioImpl :: EffectFn1 (ChildProcess) (Array Foreign)

-- | The standard output stream of a child process. Note that this is only
-- | available if the process was spawned with the stdout option set to "pipe".
stdout :: ChildProcess -> Maybe (Readable ())
stdout = toMaybe <<< stdoutImpl

foreign import stdoutImpl :: ChildProcess -> Nullable (Readable ())

-- | Send messages to the (`nodejs`) child process.
-- |
-- | See the [node documentation](https://nodejs.org/api/child_process.html#child_process_subprocess_send_message_sendhandle_options_callback)
-- | for in-depth documentation.
-- send
--   :: forall props
--    . { | props }
--   -> Handle
--   -> ChildProcess
--   -> Effect Boolean
-- send msg handle (ChildProcess cp) = mkEffect \_ -> runFn2 cp.send msg handle

-- | Handle the `"close"` signal.
onClose :: ChildProcess -> (Maybe Int -> Maybe String -> Effect Unit) -> Effect Unit
onClose cp cb = runEffectFn2 onCloseImpl cp $ mkEffectFn2 \a b -> cb (toMaybe a) (toMaybe b)

foreign import onCloseImpl :: EffectFn2 (ChildProcess) (EffectFn2 (Nullable Int) (Nullable String) Unit) (Unit)

-- | Handle the `"disconnect"` signal.
onDisconnect :: ChildProcess -> Effect Unit -> Effect Unit
onDisconnect cp cb = runEffectFn2 onDisconnectImpl cp cb

foreign import onDisconnectImpl :: EffectFn2 (ChildProcess) (Effect Unit) (Unit)

-- | Handle the `"error"` signal.
onError :: ChildProcess -> (Error -> Effect Unit) -> Effect Unit
onError cp cb = runEffectFn2 onErrorImpl cp $ mkEffectFn1 cb

foreign import onErrorImpl :: EffectFn2 (ChildProcess) (EffectFn1 Error Unit) (Unit)

-- | Handle the `"exit"` signal.
onExit :: ChildProcess -> ({ exitCode :: Maybe Int, signalCode :: Maybe { code :: String, signal :: Maybe Signal } } -> Effect Unit) -> Effect Unit
onExit cp cb = runEffectFn2 onExitImpl cp $ mkEffectFn2 \a b ->
  cb { exitCode: toMaybe a, signalCode: toMaybe b <#> \s -> { code: s, signal: Signal.fromString s } }

foreign import onExitImpl :: EffectFn2 (ChildProcess) (EffectFn2 (Nullable Int) (Nullable String) Unit) (Unit)

-- | Handle the `"message"` signal.
onMessage :: ChildProcess -> (Foreign -> Maybe Handle -> Effect Unit) -> Effect Unit
onMessage cp cb = runEffectFn2 onMessageImpl cp $ mkEffectFn2 \a b -> cb a (toMaybe b)

foreign import onMessageImpl :: EffectFn2 (ChildProcess) (EffectFn2 Foreign (Nullable Handle) Unit) (Unit)

onSpawn :: ChildProcess -> Effect Unit -> Effect Unit
onSpawn cp cb = runEffectFn2 onSpawnImpl cp cb

foreign import onSpawnImpl :: EffectFn2 ChildProcess (Effect Unit) Unit

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

exec :: String -> ({ error :: Maybe Exception.Error, stdout :: ImmutableBuffer, stderr :: ImmutableBuffer } -> Effect Unit) -> Effect ChildProcess
exec cmd = exec' cmd identity

-- | Similar to `spawn`, except that this variant will:
-- | * run the given command with the shell,
-- | * buffer output, and wait until the process has exited before calling the
-- |   callback.
-- |
-- | Note that the child process will be killed if the amount of output exceeds
-- | a certain threshold (the default is defined by Node.js).
exec' :: String -> (ExecOptions -> ExecOptions) -> ({ error :: Maybe Exception.Error, stdout :: ImmutableBuffer, stderr :: ImmutableBuffer } -> Effect Unit) -> Effect ChildProcess
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

foreign import execImpl :: EffectFn3 String JsExecOptions (EffectFn3 (Nullable Exception.Error) ImmutableBuffer ImmutableBuffer Unit) ChildProcess

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
  , shell :: ActualShellOption
  , signal :: AbortSignal
  , timeout :: Int
  , maxBuffer :: Int
  , killSignal :: KillSignal
  , uid :: Int
  , gid :: Int
  , windowsHide :: Boolean
  , windowsVerbatimArguments :: Boolean
  }

execFile :: String -> Array String -> ({ error :: Maybe Exception.Error, stdout :: ImmutableBuffer, stderr :: ImmutableBuffer } -> Effect Unit) -> Effect ChildProcess
execFile file args = execFile' file args identity

-- | Like `exec`, except instead of using a shell, it passes the arguments
-- | directly to the specified command.
execFile' :: String -> Array String -> (ExecFileOptions -> ExecFileOptions) -> ({ error :: Maybe Exception.Error, stdout :: ImmutableBuffer, stderr :: ImmutableBuffer } -> Effect Unit) -> Effect ChildProcess
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

foreign import execFileImpl :: EffectFn4 (String) (Array String) (JsExecFileOptions) (EffectFn3 (Nullable Exception.Error) ImmutableBuffer ImmutableBuffer Unit) (ChildProcess)

type ExecSyncOptions =
  { cwd :: Maybe String
  , input :: Maybe ImmutableBuffer
  , stdio :: Maybe (Array (Maybe StdIOBehaviour))
  , env :: Maybe (Object String)
  , shell :: Maybe String
  , uid :: Maybe Int
  , gid :: Maybe Int
  , timeout :: Maybe Int
  , killSignal :: Maybe (Either String Int)
  , maxBuffer :: Maybe Int
  , signal :: Maybe AbortSignal
  , windowsHide :: Maybe Boolean
  }

type JsExecSyncOptions =
  { cwd :: String
  , input :: ImmutableBuffer
  , stdio :: ActualStdIOOptions
  , env :: Object String
  , encoding :: String
  , shell :: String
  , uid :: Int
  , gid :: Int
  , timeout :: Int
  , killSignal :: KillSignal
  , maxBuffer :: Int
  , signal :: AbortSignal
  , windowsHide :: Boolean
  }

execSync :: String -> Effect ImmutableBuffer
execSync cmd = execSync' cmd identity

-- | Generally identical to `exec`, with the exception that
-- | the method will not return until the child process has fully closed.
-- | Returns: The stdout from the command.
execSync' :: String -> (ExecSyncOptions -> ExecSyncOptions) -> Effect ImmutableBuffer
execSync' cmd buildOptions = runEffectFn2 execSyncImpl cmd jsOptions
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , input: fromMaybe undefined options.input
    , stdio: maybe undefined toActualStdIOOptions options.stdio
    , env: fromMaybe undefined options.env
    , encoding: "buffer" -- force stdout/stderr in callback to be Buffers
    , shell: fromMaybe undefined options.shell
    , uid: fromMaybe undefined options.uid
    , gid: fromMaybe undefined options.gid
    , timeout: fromMaybe undefined options.timeout
    , killSignal: fromMaybe undefined $ map (either (unsafeCoerce :: String -> KillSignal) (unsafeCoerce :: Int -> KillSignal)) options.killSignal
    , maxBuffer: fromMaybe undefined options.maxBuffer
    , signal: fromMaybe undefined options.signal
    , windowsHide: fromMaybe undefined options.windowsHide
    }

  defaults :: ExecSyncOptions
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
    , signal: Nothing
    , windowsHide: Nothing
    }

foreign import execSyncImpl :: EffectFn2 String JsExecSyncOptions ImmutableBuffer

type ExecFileSyncOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , input :: Maybe ImmutableBuffer
  , stdio :: Maybe (Array (Maybe StdIOBehaviour))
  , shell :: Maybe String
  , uid :: Maybe Int
  , gid :: Maybe Int
  , timeout :: Maybe Int
  , killSignal :: Maybe (Either String Int)
  , maxBuffer :: Maybe Int
  , windowsHide :: Maybe Boolean
  }

type JsExecFileSyncOptions =
  { cwd :: String
  , input :: ImmutableBuffer
  , stdio :: ActualStdIOOptions
  , env :: Object String
  , uid :: Int
  , gid :: Int
  , timeout :: Int
  , killSignal :: KillSignal
  , maxBuffer :: Int
  , encoding :: String
  , windowsHide :: Boolean
  , shell :: String
  }

execFileSync :: String -> Array String -> Effect ImmutableBuffer
execFileSync file args = execFileSync' file args identity

-- | Generally identical to `execFile`, with the exception that
-- | the method will not return until the child process has fully closed.
-- | Returns: The stdout from the command.
execFileSync' :: String -> Array String -> (ExecFileSyncOptions -> ExecFileSyncOptions) -> Effect ImmutableBuffer
execFileSync' file args buildOptions = runEffectFn3 execFileSyncImpl file args jsOptions
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , input: fromMaybe undefined options.input
    , stdio: maybe undefined toActualStdIOOptions options.stdio
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

  defaults :: ExecFileSyncOptions
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

foreign import execFileSyncImpl :: EffectFn3 String (Array String) JsExecFileSyncOptions ImmutableBuffer

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

type SpawnOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , argv0 :: Maybe String
  , stdio :: Maybe (Array (Maybe StdIOBehaviour))
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

type JsSpawnOptions =
  { cwd :: String
  , env :: Object String
  , argv0 :: String
  , stdio :: ActualStdIOOptions
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

spawn :: String -> Array String -> Effect ChildProcess
spawn file args = spawn' file args identity

-- | Spawn a child process. Note that, in the event that a child process could
-- | not be spawned (for example, if the executable was not found) this will
-- | not throw an error. Instead, the `ChildProcess` will be created anyway,
-- | but it will immediately emit an 'error' event.
spawn' :: String -> Array String -> (SpawnOptions -> SpawnOptions) -> Effect ChildProcess
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
    , stdio: maybe undefined toActualStdIOOptions options.stdio
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

foreign import spawnImpl :: EffectFn3 String (Array String) JsSpawnOptions ChildProcess

type SpawnSyncOptions =
  { cwd :: Maybe String
  , input :: Maybe ImmutableBuffer
  , argv0 :: Maybe String
  , stdio :: Maybe (Array (Maybe StdIOBehaviour))
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

type JsSpawnSyncOptions =
  { cwd :: String
  , input :: ImmutableBuffer
  , argv0 :: String
  , stdio :: ActualStdIOOptions
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

spawnSync :: String -> Array String -> Effect ChildProcess
spawnSync file args = spawnSync' file args identity

spawnSync' :: String -> Array String -> (SpawnSyncOptions -> SpawnSyncOptions) -> Effect ChildProcess
spawnSync' file args buildOptions = runEffectFn3 spawnSyncImpl file args jsOptions
  where
  options = buildOptions defaults
  jsOptions =
    { cwd: fromMaybe undefined options.cwd
    , input: fromMaybe undefined options.input
    , argv0: fromMaybe undefined options.argv0
    , stdio: maybe undefined toActualStdIOOptions options.stdio
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

  defaults :: SpawnSyncOptions
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

foreign import spawnSyncImpl :: EffectFn3 String (Array String) JsSpawnSyncOptions ChildProcess

type ForkOptions =
  { cwd :: Maybe String
  , detached :: Maybe Boolean
  , env :: Maybe (Object String)
  , execPath :: Maybe String
  , execArgv :: Maybe (Array String)
  , gid :: Maybe Int
  , serialization :: Maybe SerializationOption
  , signal :: Maybe AbortSignal
  , stdio :: Maybe (Array (Maybe StdIOBehaviour))
  , uid :: Maybe Int
  , windowsVerbatimArguments :: Maybe Boolean
  , timeout :: Maybe Int
  }

type JsForkOptions =
  { cwd :: String
  , detached :: Boolean
  , env :: Object String
  , execPath :: String
  , execArgv :: Array String
  , gid :: Int
  , serialization :: String
  , signal :: AbortSignal
  , stdio :: ActualStdIOOptions
  , uid :: Int
  , windowsVerbatimArguments :: Boolean
  , timeout :: Int
  }

fork :: String -> Array String -> Effect ChildProcess
fork modulePath args = fork' modulePath args identity

-- | A special case of `spawn` for creating Node.js child processes. The first
-- | argument is the module to be run, and the second is the argv (command line
-- | arguments).
fork' :: String -> Array String -> (ForkOptions -> ForkOptions) -> Effect ChildProcess
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
    , stdio: maybe undefined toActualStdIOOptions options.stdio
    , uid: fromMaybe undefined options.uid
    , windowsVerbatimArguments: fromMaybe undefined options.windowsVerbatimArguments
    , timeout: fromMaybe undefined options.timeout
    }

  defaults =
    { cwd: Nothing
    , detached: Nothing
    , env: Nothing
    , execPath: Nothing
    , execArgv: Nothing
    , gid: Nothing
    , serialization: Nothing
    , signal: Nothing
    , stdio: Nothing
    , uid: Nothing
    , windowsVerbatimArguments: Nothing
    , timeout: Nothing
    }

foreign import forkImpl :: EffectFn3 String (Array String) JsForkOptions ChildProcess

foreign import undefined :: forall a. a

foreign import data ActualExecSyncOptions :: Type

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
-- | * `Overlapped`: Same as `pipe` on `non-Windows` OSes. For Windows, see
-- |    https://learn.microsoft.com/en-us/windows/win32/fileio/synchronous-and-asynchronous-i-o
-- | * `Ipc`: Enables `send`/`disconnect`/`onDisconnect`/`onMessage`. Only 1 `Ipd` per `stdio` file descriptor.
data StdIOBehaviour
  = Pipe
  | Ignore
  | ShareStream (forall r. Stream r)
  | ShareFD FS.FileDescriptor
  | Overlapped
  | Ipc

-- | Create pipes for each of the three standard IO streams.
pipe :: Array (Maybe StdIOBehaviour)
pipe = map Just [ Pipe, Pipe, Pipe ]

-- | Share `stdin` with `stdin`, `stdout` with `stdout`,
-- | and `stderr` with `stderr`.
inherit :: Array (Maybe StdIOBehaviour)
inherit = map Just
  [ inheritStdin
  , inheritStdout
  , inheritStderr
  ]

-- | Set the stdin/stdout/stderr or fallback to inherit the process's stdin/stdout/stderr.
inheritOr
  :: { stdin :: StdIOBehaviour -> Maybe StdIOBehaviour
     , stdout :: StdIOBehaviour -> Maybe StdIOBehaviour
     , stderr :: StdIOBehaviour -> Maybe StdIOBehaviour
     }
  -> Array (Maybe StdIOBehaviour)
inheritOr r =
  [ r.stdin inheritStdin
  , r.stdout inheritStdout
  , r.stderr inheritStderr
  ]

inheritStdin :: StdIOBehaviour
inheritStdin = ShareStream process.stdin

inheritStdout :: StdIOBehaviour
inheritStdout = ShareStream process.stdout

inheritStderr :: StdIOBehaviour
inheritStderr = ShareStream process.stderr

foreign import process :: forall props. { | props }

-- | Ignore all streams.
ignore :: Array (Maybe StdIOBehaviour)
ignore = map Just [ Ignore, Ignore, Ignore ]

stdIOBehavior :: { stdin :: Maybe StdIOBehaviour, stdout :: Maybe StdIOBehaviour, stderr :: Maybe StdIOBehaviour } -> Array (Maybe StdIOBehaviour)
stdIOBehavior r = [ r.stdin, r.stdout, r.stderr ]

-- Helpers

foreign import data ActualStdIOBehaviour :: Type

toActualStdIOBehaviour :: StdIOBehaviour -> ActualStdIOBehaviour
toActualStdIOBehaviour b = case b of
  Pipe -> c "pipe"
  Ignore -> c "ignore"
  ShareFD x -> c x
  ShareStream stream -> c stream
  Overlapped -> c "overlapped"
  Ipc -> c "ipc"
  where
  c :: forall a. a -> ActualStdIOBehaviour
  c = unsafeCoerce

type ActualStdIOOptions = Array (Nullable ActualStdIOBehaviour)

toActualStdIOOptions :: Array (Maybe StdIOBehaviour) -> ActualStdIOOptions
toActualStdIOOptions = map (toNullable <<< map toActualStdIOBehaviour)
