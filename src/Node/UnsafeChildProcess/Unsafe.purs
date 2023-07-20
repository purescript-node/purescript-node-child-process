-- | Exposes low-level functions for ChildProcess
-- | where JavaScript values, rather than PureScript ones,
-- | are expected.
-- |
-- | All functions prefixed with `unsafe` indicate why they can be unsafe
-- | (i.e. produce a crash a runtime). All other functions
-- | are unsafe because their options (or default ones if not specified)
-- | can affect whether the `unsafe*` values/methods exist.
-- |
-- | All type aliases for options (e.g. `ExecSyncOptions`) are well-typed.
module Node.UnsafeChildProcess.Unsafe
  ( unsafeSOBToString
  , unsafeSOBToBuffer
  , unsafeStdin
  , unsafeStdout
  , unsafeStderr
  , execSync
  , ExecSyncOptions
  , execSyncOpts
  , exec
  , ExecOptions
  , execOpts
  , execCb
  , execOptsCb
  , execFileSync
  , ExecFileSyncOptions
  , execFileSync'
  , execFile
  , ExecFileOptions
  , execFileOpts
  , execFileCb
  , execFileOptsCb
  , SpawnSyncResult
  , spawnSync
  , SpawnSyncOptions
  , spawnSync'
  , spawn
  , SpawnOptions
  , spawn'
  , fork
  , ForkOptions
  , fork'
  , unsafeSend
  , SendOptions
  , unsafeSendOpts
  , unsafeSendCb
  , unsafeSendOptsCb
  , unsafeChannelRef
  , unsafeChannelUnref
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Posix (Gid, Pid, Uid)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, mkEffectFn1, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Foreign (Foreign)
import Foreign.Object (Object)
import Node.Buffer (Buffer)
import Node.ChildProcess.Types (Handle, KillSignal, Shell, StdIO, StringOrBuffer, UnsafeChildProcess)
import Node.Errors.SystemError (SystemError)
import Node.Stream (Readable, Writable)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

-- | Same as `unsafeCoerce`. No runtime checking is done to ensure
-- | the value is a `String`.
unsafeSOBToString :: StringOrBuffer -> String
unsafeSOBToString = unsafeCoerce

-- | Same as `unsafeCoerce`. No runtime checking is done to ensure
-- | the value is a `Buffer`.
unsafeSOBToBuffer :: StringOrBuffer -> Buffer
unsafeSOBToBuffer = unsafeCoerce

-- | Unsafe because it depends on what value was passed in via `stdio[0]`
foreign import unsafeStdin :: UnsafeChildProcess -> Nullable (Writable ())
-- | Unsafe because it depends on what value was passed in via `stdio[1]`
foreign import unsafeStdout :: UnsafeChildProcess -> Nullable (Readable ())
-- | Unsafe because it depends on what value was passed in via `stdio[2]`
foreign import unsafeStderr :: UnsafeChildProcess -> Nullable (Readable ())

execSync :: String -> Effect StringOrBuffer
execSync command = runEffectFn1 execSyncImpl command

foreign import execSyncImpl :: EffectFn1 (String) (StringOrBuffer)

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `input` <string> | <Buffer> | <TypedArray> | <DataView> The value which will be passed as stdin to the spawned process. Supplying this value will override stdio[0].
-- | - `stdio` <string> | <Array> Child's stdio configuration. stderr by default will be output to the parent process' stderr unless stdio is specified. Default: 'pipe'.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `shell` <string> Shell to execute the command with. See Shell requirements and Default Windows shell. Default: '/bin/sh' on Unix, process.env.ComSpec on Windows.
-- | - `uid` <number> Sets the user identity of the process. (See setuid(2)).
-- | - `gid` <number> Sets the group identity of the process. (See setgid(2)).
-- | - `timeout` <number> In milliseconds the maximum amount of time the process is allowed to run. Default: undefined.
-- | - `killSignal` <string> | <integer> The signal value to be used when the spawned process will be killed. Default: 'SIGTERM'.
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated and any output is truncated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `encoding` <string> The encoding used for all stdio inputs and outputs. Default: 'buffer'.
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
type ExecSyncOptions =
  ( cwd :: String
  , input :: Buffer
  , stdio :: Array StdIO
  , env :: Object String
  , shell :: String
  , uid :: Uid
  , gid :: Gid
  , timeout :: Milliseconds
  , killSignal :: KillSignal
  , maxBuffer :: Number
  , encoding :: String
  , windowsHide :: Boolean
  )

execSyncOpts
  :: forall r trash
   . Row.Union r trash ExecSyncOptions
  => String
  -> { | r }
  -> Effect StringOrBuffer
execSyncOpts command opts = runEffectFn2 execSyncOptsImpl command opts

foreign import execSyncOptsImpl :: forall r. EffectFn2 (String) ({ | r }) (StringOrBuffer)

exec :: String -> Effect UnsafeChildProcess
exec command = runEffectFn1 execImpl command

foreign import execImpl :: EffectFn1 (String) (UnsafeChildProcess)

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `encoding` <string> Default: 'utf8'
-- | - `timeout` <number> Default: 0
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated and any output is truncated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `killSignal` <string> | <integer> Default: 'SIGTERM'
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
type ExecOptions =
  ( cwd :: String
  , env :: Object String
  , encoding :: String
  , timeout :: Number
  , maxBuffer :: Number
  , killSignal :: KillSignal
  , uid :: Uid
  , gid :: Gid
  , windowsHide :: Boolean
  , shell :: Shell
  )

execOpts
  :: forall r trash
   . Row.Union r trash ExecOptions
  => String
  -> { | r }
  -> Effect UnsafeChildProcess
execOpts command opts = runEffectFn2 execOptsImpl command opts

foreign import execOptsImpl :: forall r. EffectFn2 (String) ({ | r }) (UnsafeChildProcess)

execCb :: String -> (SystemError -> StringOrBuffer -> StringOrBuffer -> Effect Unit) -> Effect UnsafeChildProcess
execCb command cb = runEffectFn2 execCbImpl command $ mkEffectFn3 cb

foreign import execCbImpl :: EffectFn2 (String) (EffectFn3 SystemError StringOrBuffer StringOrBuffer Unit) (UnsafeChildProcess)

execOptsCb
  :: forall r trash
   . Row.Union r trash ExecOptions
  => String
  -> { | r }
  -> (SystemError -> StringOrBuffer -> StringOrBuffer -> Effect Unit)
  -> Effect UnsafeChildProcess
execOptsCb command opts cb = runEffectFn3 execOptsCbImpl command opts $ mkEffectFn3 cb

foreign import execOptsCbImpl :: forall r. EffectFn3 (String) ({ | r }) (EffectFn3 SystemError StringOrBuffer StringOrBuffer Unit) (UnsafeChildProcess)

execFileSync :: String -> Array String -> Effect StringOrBuffer
execFileSync file args = runEffectFn2 execFileSyncImpl file args

foreign import execFileSyncImpl :: EffectFn2 (String) (Array String) (StringOrBuffer)

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `input` <string> | <Buffer> | <TypedArray> | <DataView> The value which will be passed as stdin to the spawned process. Supplying this value will override stdio[0].
-- | - `stdio` <string> | <Array> Child's stdio configuration. stderr by default will be output to the parent process' stderr unless stdio is specified. Default: 'pipe'.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `timeout` <number> In milliseconds the maximum amount of time the process is allowed to run. Default: undefined.
-- | - `killSignal` <string> | <integer> The signal value to be used when the spawned process will be killed. Default: 'SIGTERM'.
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `encoding` <string> The encoding used for all stdio inputs and outputs. Default: 'buffer'.
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
type ExecFileSyncOptions =
  ( cwd :: String
  , input :: Buffer
  , stdio :: Array StdIO
  , env :: Object String
  , uid :: Uid
  , gid :: Gid
  , timeout :: Milliseconds
  , killSignal :: KillSignal
  , maxBuffer :: Number
  , encoding :: String
  , windowsHide :: Boolean
  , shell :: Shell
  )

execFileSync'
  :: forall r trash
   . Row.Union r trash ExecFileSyncOptions
  => String
  -> Array String
  -> { | r }
  -> Effect StringOrBuffer
execFileSync' file args options = runEffectFn3 execFileSyncOptsImpl file args options

foreign import execFileSyncOptsImpl :: forall r. EffectFn3 (String) (Array String) ({ | r }) (StringOrBuffer)

execFile :: String -> Array String -> Effect UnsafeChildProcess
execFile file args = runEffectFn2 execFileImpl file args

foreign import execFileImpl :: EffectFn2 (String) (Array String) (UnsafeChildProcess)

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `encoding` <string> Default: 'utf8'
-- | - `timeout` <number> Default: 0
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated and any output is truncated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `killSignal` <string> | <integer> Default: 'SIGTERM'
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
-- | - `windowsVerbatimArguments` <boolean> No quoting or escaping of arguments is done on Windows. Ignored on Unix. Default: false.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
type ExecFileOptions =
  ( cwd :: String
  , env :: Object String
  , encoding :: String
  , timeout :: Number
  , maxBuffer :: Number
  , killSignal :: KillSignal
  , uid :: Uid
  , gid :: Gid
  , windowsHide :: Boolean
  , windowsVerbatimArguments :: Boolean
  , shell :: Shell
  )

execFileOpts
  :: forall r trash
   . Row.Union r trash ExecFileOptions
  => String
  -> Array String
  -> { | r }
  -> Effect UnsafeChildProcess
execFileOpts file args opts = runEffectFn3 execFileOptsImpl file args opts

foreign import execFileOptsImpl :: forall r. EffectFn3 (String) (Array String) ({ | r }) (UnsafeChildProcess)

execFileCb :: String -> Array String -> (SystemError -> StringOrBuffer -> StringOrBuffer -> Effect Unit) -> Effect UnsafeChildProcess
execFileCb file args cb = runEffectFn3 execFileCbImpl file args $ mkEffectFn3 cb

foreign import execFileCbImpl :: EffectFn3 (String) (Array String) (EffectFn3 SystemError StringOrBuffer StringOrBuffer Unit) (UnsafeChildProcess)

execFileOptsCb
  :: forall r trash
   . Row.Union r trash ExecFileOptions
  => String
  -> Array String
  -> { | r }
  -> (SystemError -> StringOrBuffer -> StringOrBuffer -> Effect Unit)
  -> Effect UnsafeChildProcess
execFileOptsCb file args opts cb = runEffectFn4 execFileOptsCbImpl file args opts $ mkEffectFn3 cb

foreign import execFileOptsCbImpl :: forall r. EffectFn4 (String) (Array String) ({ | r }) (EffectFn3 SystemError StringOrBuffer StringOrBuffer Unit) (UnsafeChildProcess)

type SpawnSyncResult =
  { pid :: Pid
  , output :: Array Foreign
  , stdout :: StringOrBuffer
  , stderr :: StringOrBuffer
  , status :: Nullable Int
  , signal :: Nullable String
  , error :: SystemError
  }

spawnSync :: String -> Array String -> Effect SpawnSyncResult
spawnSync command args = runEffectFn2 spawnSyncImpl command args

foreign import spawnSyncImpl :: EffectFn2 (String) (Array String) (SpawnSyncResult)

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `input` <string> | <Buffer> | <TypedArray> | <DataView> The value which will be passed as stdin to the spawned process. Supplying this value will override stdio[0].
-- | - `argv0` <string> Explicitly set the value of argv[0] sent to the child process. This will be set to command if not specified.
-- | - `stdio` <string> | <Array> Child's stdio configuration.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `timeout` <number> In milliseconds the maximum amount of time the process is allowed to run. Default: undefined.
-- | - `killSignal` <string> | <integer> The signal value to be used when the spawned process will be killed. Default: 'SIGTERM'.
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated and any output is truncated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `encoding` <string> The encoding used for all stdio inputs and outputs. Default: 'buffer'.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
-- | - `windowsVerbatimArguments` <boolean> No quoting or escaping of arguments is done on Windows. Ignored on Unix. This is set to true automatically when shell is specified and is CMD. Default: false.
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
type SpawnSyncOptions =
  ( cwd :: String
  , input :: Buffer
  , argv0 :: String
  , stdio :: Array StdIO
  , env :: Object String
  , uid :: Uid
  , gid :: Gid
  , timeout :: Milliseconds
  , killSignal :: KillSignal
  , maxBuffer :: Number
  , encoding :: String
  , shell :: Shell
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  )

spawnSync'
  :: forall r trash
   . Row.Union r trash SpawnSyncOptions
  => String
  -> Array String
  -> { | r }
  -> Effect SpawnSyncResult
spawnSync' command args opts = runEffectFn3 spawnSyncOptsImpl command args opts

foreign import spawnSyncOptsImpl :: forall r. EffectFn3 (String) (Array String) ({ | r }) (SpawnSyncResult)

spawn :: String -> Array String -> Effect UnsafeChildProcess
spawn command args = runEffectFn2 spawnImpl command args

foreign import spawnImpl :: EffectFn2 (String) (Array String) (UnsafeChildProcess)

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `argv0` <string> Explicitly set the value of argv[0] sent to the child process. This will be set to command if not specified.
-- | - `stdio` <Array> | <string> Child's stdio configuration (see options.stdio).
-- | - `detached` <boolean> Prepare child to run independently of its parent process. Specific behavior depends on the platform, see options.detached).
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `serialization` <string> Specify the kind of serialization used for sending messages between processes. Possible values are 'json' and 'advanced'. See Advanced serialization for more details. Default: 'json'.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
-- | - `windowsVerbatimArguments` <boolean> No quoting or escaping of arguments is done on Windows. Ignored on Unix. This is set to true automatically when shell is specified and is CMD. Default: false.
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
-- | - `signal` <AbortSignal> allows aborting the child process using an AbortSignal.
-- | - `timeout` <number> In milliseconds the maximum amount of time the process is allowed to run. Default: undefined.
-- | - `killSignal` <string> | <integer> The signal value to be used when the spawned process will be killed by timeout or abort signal. Default: 'SIGTERM'.
type SpawnOptions =
  ( cwd :: String
  , env :: Object String
  , argv0 :: String
  , stdio :: Array StdIO
  , detached :: Boolean
  , uid :: Uid
  , gid :: Gid
  , serialization :: String
  , shell :: Shell
  , windowsVerbatimArguments :: Boolean
  , windowsHide :: Boolean
  , timeout :: Number
  , killSignal :: KillSignal
  )

spawn'
  :: forall r trash
   . Row.Union r trash SpawnOptions
  => String
  -> Array String
  -> { | r }
  -> Effect UnsafeChildProcess
spawn' command args opts = runEffectFn3 spawnOptsImpl command args opts

foreign import spawnOptsImpl :: forall r. EffectFn3 (String) (Array String) ({ | r }) (UnsafeChildProcess)

fork :: String -> Array String -> Effect UnsafeChildProcess
fork modulePath args = runEffectFn2 forkImpl modulePath args

foreign import forkImpl :: EffectFn2 (String) (Array String) (UnsafeChildProcess)

type ForkOptions =
  ( cwd :: String
  , detached :: Boolean
  , env :: Object String
  , execPath :: String
  , execArgv :: Array String
  , gid :: Gid
  , serialization :: String
  , killSignal :: KillSignal
  , silent :: Boolean
  , stdio :: Array StdIO
  , uid :: Uid
  , windowsVerbatimArguments :: Boolean
  , timeout :: Milliseconds
  )

fork'
  :: forall r trash
   . Row.Union r trash ForkOptions
  => String
  -> Array String
  -> { | r }
  -> Effect UnsafeChildProcess
fork' modulePath args opts = runEffectFn3 forkOptsImpl modulePath args opts

foreign import forkOptsImpl :: forall r. EffectFn3 (String) (Array String) { | r } (UnsafeChildProcess)

-- | Unsafe because child process must be a Node child process and an IPC channel must exist.
unsafeSend :: Object Foreign -> Nullable Handle -> UnsafeChildProcess -> Effect Boolean
unsafeSend msg handle cp = runEffectFn3 sendImpl cp msg handle

foreign import sendImpl :: EffectFn3 (UnsafeChildProcess) (Object Foreign) (Nullable Handle) (Boolean)

type SendOptions =
  ( keepAlive :: Boolean
  )

-- | Unsafe because child process must be a Node child process and an IPC channel must exist.
unsafeSendOpts
  :: forall r trash
   . Row.Union r trash SendOptions
  => Object Foreign
  -> Nullable Handle
  -> { | r }
  -> UnsafeChildProcess
  -> Effect Boolean
unsafeSendOpts msg handle opts cp = runEffectFn4 sendOptsImpl cp msg handle opts

foreign import sendOptsImpl :: forall r. EffectFn4 (UnsafeChildProcess) (Object Foreign) (Nullable Handle) ({ | r }) (Boolean)

-- | Unsafe because child process must be a Node child process and an IPC channel must exist.
unsafeSendCb :: Object Foreign -> Nullable Handle -> (Maybe Error -> Effect Unit) -> UnsafeChildProcess -> Effect Boolean
unsafeSendCb msg handle cb cp = runEffectFn4 sendCbImpl cp msg handle $ mkEffectFn1 \err -> cb $ toMaybe err

foreign import sendCbImpl :: EffectFn4 (UnsafeChildProcess) (Object Foreign) (Nullable Handle) (EffectFn1 (Nullable Error) Unit) (Boolean)

-- | Unsafe because child process must be a Node child process and an IPC channel must exist.
unsafeSendOptsCb
  :: forall r trash
   . Row.Union r trash SendOptions
  => Object Foreign
  -> Nullable Handle
  -> { | r }
  -> (Maybe Error -> Effect Unit)
  -> UnsafeChildProcess
  -> Effect Boolean
unsafeSendOptsCb msg handle opts cb cp = runEffectFn5 sendOptsCbImpl cp msg handle opts $ mkEffectFn1 \err -> cb $ toMaybe err

foreign import sendOptsCbImpl :: forall r. EffectFn5 (UnsafeChildProcess) (Object Foreign) (Nullable Handle) ({ | r }) (EffectFn1 (Nullable Error) Unit) (Boolean)

-- | Unsafe because it depends on whether an IPC channel exists.
unsafeChannelRef :: UnsafeChildProcess -> Effect Unit
unsafeChannelRef cp = runEffectFn1 unsafeChannelRefImpl cp

foreign import unsafeChannelRefImpl :: EffectFn1 (UnsafeChildProcess) (Unit)

-- | Unsafe because it depends on whether an IPC channel exists.
unsafeChannelUnref :: UnsafeChildProcess -> Effect Unit
unsafeChannelUnref cp = runEffectFn1 unsafeChannelUnrefImpl cp

foreign import unsafeChannelUnrefImpl :: EffectFn1 (UnsafeChildProcess) (Unit)
