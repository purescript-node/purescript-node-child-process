-- | This module contains various types and functions to allow you to spawn and
-- | interact with child processes.
-- |
-- | It is intended to be imported qualified, as follows:
-- |
-- | ```purescript
-- | import Node.ChildProcess as ChildProcess
-- | -- or...
-- | import Node.ChildProcess as CP
-- | ```
-- |
-- | The [Node.js documentation](https://nodejs.org/api/child_process.html)
-- | forms the basis for this module and has in-depth documentation about
-- | runtime behaviour.
-- |
-- | ## Meaning of `appendStdio`
-- |
-- | By default, `ChildProcess` uses `safeStdio` for its `stdio` option. However,
-- | Node allows one to pass in additional values besides the typical 3 (i.e. `stdin`, `stdout`, `stderr`)
-- | and the IPC channel that might be used (i.e. `ipc`). Thus, `appendStdio` is an option
-- | defined in this library that doesn't exist in the Node docs.
-- | It exists to allow the end-user to append additional values to the `safeStdio` value
-- | used here. For example,
-- | 
-- | ```
-- | spawn' file args (_ { appendStdio = Just [ fileDescriptor8, pipe, pipe ]})
-- | ```
-- |
-- | would end up calling `spawn` with the following `stdio`:
-- | ```
-- | -- i.e. `safeStdio <> [ fileDescriptor8, pipe, pipe ]`
-- | [pipe, pipe, pipe, ipc, fileDescriptor8, pipe, pipe]
-- | ```
module Node.ChildProcess
  ( ChildProcess
  , toEventEmitter
  , toUnsafeChildProcess
  , closeH
  , disconnectH
  , errorH
  , exitH
  , messageH
  , spawnH
  , stdin
  , stdout
  , stderr
  , pid
  , pidExists
  , connected
  , disconnect
  , exitCode
  , kill
  , kill'
  , killSignal
  , killed
  , ref
  , unref
  , signalCode
  , spawnFile
  , spawnArgs
  , stdio
  , spawnSync
  , SpawnSyncOptions
  , spawnSync'
  , spawn
  , SpawnOptions
  , spawn'
  , execSync
  , ExecSyncOptions
  , execSync'
  , exec
  , ExecResult
  , ExecOptions
  , exec'
  , execFileSync
  , ExecFileSyncOptions
  , execFileSync'
  , execFile
  , ExecFileOptions
  , execFile'
  , fork
  , fork'
  , send
  , send'
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Posix (Pid, Gid, Uid)
import Data.Posix.Signal (Signal)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Foreign.Object (Object)
import Node.Buffer (Buffer)
import Node.ChildProcess.Types (Exit(..), Handle, KillSignal, Shell, StdIO, UnsafeChildProcess, ipc, pipe)
import Node.Errors.SystemError (SystemError)
import Node.EventEmitter (EventEmitter, EventHandle)
import Node.EventEmitter.UtilTypes (EventHandle0, EventHandle1)
import Node.Stream (Readable, Writable)
import Node.UnsafeChildProcess.Safe as SafeCP
import Node.UnsafeChildProcess.Unsafe (unsafeSOBToBuffer)
import Node.UnsafeChildProcess.Unsafe as UnsafeCP
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- | Opaque type returned by `spawn`, `fork` and `exec`.
-- | Needed as input for most methods in this module.
-- |
-- | `ChildProcess` extends `EventEmitter`
newtype ChildProcess = ChildProcess UnsafeChildProcess

toEventEmitter :: ChildProcess -> EventEmitter
toEventEmitter = toUnsafeChildProcess >>> SafeCP.toEventEmitter

toUnsafeChildProcess :: ChildProcess -> UnsafeChildProcess
toUnsafeChildProcess (ChildProcess p) = p

closeH :: EventHandle ChildProcess (Exit -> Effect Unit) (EffectFn2 (Nullable Int) (Nullable String) Unit)
closeH = unsafeCoerce SafeCP.closeH

disconnectH :: EventHandle0 ChildProcess
disconnectH = unsafeCoerce SafeCP.disconnectH

errorH :: EventHandle1 ChildProcess SystemError
errorH = unsafeCoerce SafeCP.errorH

exitH :: EventHandle ChildProcess (Exit -> Effect Unit) (EffectFn2 (Nullable Int) (Nullable String) Unit)
exitH = unsafeCoerce SafeCP.exitH

messageH :: EventHandle ChildProcess (Foreign -> Maybe Handle -> Effect Unit) (EffectFn2 Foreign (Nullable Handle) Unit)
messageH = unsafeCoerce SafeCP.messageH

spawnH :: EventHandle0 ChildProcess
spawnH = unsafeCoerce SafeCP.spawnH

unsafeFromNull :: forall a. Nullable a -> a
unsafeFromNull = unsafeCoerce

-- | The standard input stream of a child process.
stdin :: ChildProcess -> Writable ()
stdin = toUnsafeChildProcess >>> UnsafeCP.unsafeStdin >>> unsafeFromNull

-- | The standard output stream of a child process.
stdout :: ChildProcess -> Readable ()
stdout = toUnsafeChildProcess >>> UnsafeCP.unsafeStdout >>> unsafeFromNull

-- | The standard error stream of a child process.
stderr :: ChildProcess -> Readable ()
stderr = toUnsafeChildProcess >>> UnsafeCP.unsafeStderr >>> unsafeFromNull

-- | The process ID of a child process. This will be `Nothing` until
-- | the process has spawned. Note that if the process has already
-- | exited, another process may have taken the same ID, so be careful!
pid :: ChildProcess -> Effect (Maybe Pid)
pid cp = SafeCP.pid $ toUnsafeChildProcess cp

pidExists :: ChildProcess -> Effect Boolean
pidExists cp = SafeCP.pidExists $ toUnsafeChildProcess cp

-- | Indicates whether it is still possible to send and receive
-- | messages from the child process.
connected :: ChildProcess -> Effect Boolean
connected = unsafeCoerce SafeCP.connected

exitCode :: ChildProcess -> Effect (Maybe Int)
exitCode cp = SafeCP.exitCode $ toUnsafeChildProcess cp

-- | Closes the IPC channel between parent and child.
disconnect :: ChildProcess -> Effect Unit
disconnect cp = SafeCP.disconnect $ toUnsafeChildProcess cp

kill :: ChildProcess -> Effect Boolean
kill cp = SafeCP.kill $ toUnsafeChildProcess cp

kill' :: KillSignal -> ChildProcess -> Effect Boolean
kill' sig cp = SafeCP.kill' sig $ toUnsafeChildProcess cp

-- | Send a signal to a child process. In the same way as the
-- | [unix kill(2) system call](https://linux.die.net/man/2/kill),
-- | sending a signal to a child process won't necessarily kill it.
-- |
-- | The resulting effects of this function depend on the process
-- | and the signal. They can vary from system to system.
-- | The child process might emit an `"error"` event if the signal
-- | could not be delivered.
killSignal :: Signal -> ChildProcess -> Effect Boolean
killSignal sig cp = SafeCP.killSignal sig $ toUnsafeChildProcess cp

killed :: ChildProcess -> Effect Boolean
killed cp = SafeCP.killed $ toUnsafeChildProcess cp

ref :: ChildProcess -> Effect Unit
ref cp = SafeCP.ref $ toUnsafeChildProcess cp

unref :: ChildProcess -> Effect Unit
unref cp = SafeCP.unref $ toUnsafeChildProcess cp

signalCode :: ChildProcess -> Effect (Maybe String)
signalCode cp = SafeCP.signalCode $ toUnsafeChildProcess cp

spawnArgs :: ChildProcess -> Array String
spawnArgs cp = SafeCP.spawnArgs $ toUnsafeChildProcess cp

spawnFile :: ChildProcess -> String
spawnFile cp = SafeCP.spawnFile $ toUnsafeChildProcess cp

stdio :: ChildProcess -> Array StdIO
stdio cp = SafeCP.stdio $ toUnsafeChildProcess cp

-- | Note: `exitStatus` combines the `status` and `signal` fields
-- | from the value normally returned by `spawnSync` into one value
-- | since only one of them can be non-null at the end.
type SpawnSyncResult =
  { pid :: Pid
  , output :: Array Foreign
  , stdout :: Buffer
  , stderr :: Buffer
  , exitStatus :: Exit
  , error :: Maybe SystemError
  }

spawnSync
  :: String
  -> Array String
  -> Effect SpawnSyncResult
spawnSync command args = (UnsafeCP.spawnSync command args) <#> \r ->
  { pid: r.pid
  , output: r.output
  , stdout: unsafeSOBToBuffer r.stdout
  , stderr: unsafeSOBToBuffer r.stderr
  , exitStatus: case toMaybe r.status, toMaybe r.signal of
      Just c, _ -> Normally c
      _, Just s -> BySignal s
      _, _ -> unsafeCrashWith $ "Impossible: `spawnSync` child process neither exited nor was killed."
  , error: toMaybe r.error
  }

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `input` <string> | <Buffer> | <TypedArray> | <DataView> The value which will be passed as stdin to the spawned process. Supplying this value will override stdio[0].
-- | - `argv0` <string> Explicitly set the value of argv[0] sent to the child process. This will be set to command if not specified.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `timeout` <number> In milliseconds the maximum amount of time the process is allowed to run. Default: undefined.
-- | - `killSignal` <string> | <integer> The signal value to be used when the spawned process will be killed. Default: 'SIGTERM'.
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated and any output is truncated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
-- | - `windowsVerbatimArguments` <boolean> No quoting or escaping of arguments is done on Windows. Ignored on Unix. This is set to true automatically when shell is specified and is CMD. Default: false.
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
type SpawnSyncOptions =
  { cwd :: Maybe String
  , input :: Maybe Buffer
  , appendStdio :: Maybe (Array StdIO)
  , argv0 :: Maybe String
  , env :: Maybe (Object String)
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , timeout :: Maybe Milliseconds
  , killSignal :: Maybe KillSignal
  , maxBuffer :: Maybe Number
  , shell :: Maybe Shell
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  }

spawnSync'
  :: String
  -> Array String
  -> (SpawnSyncOptions -> SpawnSyncOptions)
  -> Effect SpawnSyncResult
spawnSync' command args buildOpts = (UnsafeCP.spawnSync' command args opts) <#> \r ->
  { pid: r.pid
  , output: r.output
  , stdout: unsafeSOBToBuffer r.stdout
  , stderr: unsafeSOBToBuffer r.stderr
  , exitStatus: case toMaybe r.status, toMaybe r.signal of
      Just c, _ -> Normally c
      _, Just s -> BySignal s
      _, _ -> unsafeCrashWith $ "Impossible: `spawnSync` child process neither exited nor was killed."
  , error: toMaybe r.error
  }
  where
  opts =
    { stdio: [ pipe, pipe, pipe ] <> fromMaybe [] o.appendStdio
    , encoding: "buffer"
    , cwd: fromMaybe undefined o.cwd
    , input: fromMaybe undefined o.input
    , argv0: fromMaybe undefined o.argv0
    , env: fromMaybe undefined o.env
    , uid: fromMaybe undefined o.uid
    , gid: fromMaybe undefined o.gid
    , timeout: fromMaybe undefined o.timeout
    , killSignal: fromMaybe undefined o.killSignal
    , maxBuffer: fromMaybe undefined o.maxBuffer
    , shell: fromMaybe undefined o.shell
    , windowsVerbatimArguments: fromMaybe undefined o.windowsVerbatimArguments
    , windowsHide: fromMaybe undefined o.windowsHide
    }

  o = buildOpts
    { cwd: Nothing
    , input: Nothing
    , appendStdio: Nothing
    , argv0: Nothing
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

-- | Spawn a child process. Note that, in the event that a child process could
-- | not be spawned (for example, if the executable was not found) this will
-- | not throw an error. Instead, the `ChildProcess` will be created anyway,
-- | but it will immediately emit an 'error' event.
spawn
  :: String
  -> Array String
  -> Effect ChildProcess
spawn cmd args = coerce $ UnsafeCP.spawn cmd args

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `argv0` <string> Explicitly set the value of argv[0] sent to the child process. This will be set to command if not specified.
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
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , argv0 :: Maybe String
  , appendStdio :: Maybe (Array StdIO)
  , detached :: Maybe Boolean
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , serialization :: Maybe String
  , shell :: Maybe Shell
  , windowsVerbatimArguments :: Maybe Boolean
  , windowsHide :: Maybe Boolean
  , timeout :: Maybe Milliseconds
  , killSignal :: Maybe KillSignal
  }

spawn'
  :: String
  -> Array String
  -> (SpawnOptions -> SpawnOptions)
  -> Effect ChildProcess
spawn' cmd args buildOpts = coerce $ UnsafeCP.spawn' cmd args opts
  where
  opts =
    { stdio: [ pipe, pipe, pipe, ipc ] <> fromMaybe [] o.appendStdio
    , cwd: fromMaybe undefined o.cwd
    , env: fromMaybe undefined o.env
    , argv0: fromMaybe undefined o.argv0
    , detached: fromMaybe undefined o.detached
    , uid: fromMaybe undefined o.uid
    , gid: fromMaybe undefined o.gid
    , serialization: fromMaybe undefined o.serialization
    , shell: fromMaybe undefined o.shell
    , windowsVerbatimArguments: fromMaybe undefined o.windowsVerbatimArguments
    , windowsHide: fromMaybe undefined o.windowsHide
    , timeout: fromMaybe undefined o.timeout
    , killSignal: fromMaybe undefined o.killSignal
    }
  o = buildOpts
    { cwd: Nothing
    , env: Nothing
    , argv0: Nothing
    , appendStdio: Nothing
    , detached: Nothing
    , uid: Nothing
    , gid: Nothing
    , serialization: Nothing
    , shell: Nothing
    , windowsVerbatimArguments: Nothing
    , windowsHide: Nothing
    , timeout: Nothing
    , killSignal: Nothing
    }

-- | Generally identical to `exec`, with the exception that
-- | the method will not return until the child process has fully closed.
-- | Returns: The `stdout` from the command.
execSync
  :: String
  -> Effect Buffer
execSync cmd = map unsafeSOBToBuffer $ UnsafeCP.execSync cmd

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
  { cwd :: Maybe String
  , input :: Maybe Buffer
  , appendStdio :: Maybe (Array StdIO)
  , env :: Maybe (Object String)
  , shell :: Maybe String
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , timeout :: Maybe Milliseconds
  , killSignal :: Maybe KillSignal
  , maxBuffer :: Maybe Number
  , windowsHide :: Maybe Boolean
  }

execSync'
  :: String
  -> (ExecSyncOptions -> ExecSyncOptions)
  -> Effect Buffer
execSync' cmd buildOpts = do
  map unsafeSOBToBuffer $ UnsafeCP.execSync' cmd opts
  where
  o = buildOpts
    { cwd: Nothing
    , input: Nothing
    , appendStdio: Nothing
    , env: Nothing
    , shell: Nothing
    , uid: Nothing
    , gid: Nothing
    , timeout: Nothing
    , killSignal: Nothing
    , maxBuffer: Nothing
    , windowsHide: Nothing
    }
  opts =
    { stdio: [ pipe, pipe, pipe ] <> fromMaybe [] o.appendStdio
    , encoding: "buffer"
    , cwd: fromMaybe undefined o.cwd
    , input: fromMaybe undefined o.input
    , env: fromMaybe undefined o.env
    , shell: fromMaybe undefined o.shell
    , uid: fromMaybe undefined o.uid
    , gid: fromMaybe undefined o.gid
    , timeout: fromMaybe undefined o.timeout
    , killSignal: fromMaybe undefined o.killSignal
    , maxBuffer: fromMaybe undefined o.maxBuffer
    , windowsHide: fromMaybe undefined o.windowsHide
    }

-- | Similar to `spawn`, except that this variant will:
-- | * run the given command with the shell,
-- | * buffer output, and wait until the process has exited.
-- |
-- | Note that the child process will be killed if the amount of output exceeds
-- | a certain threshold (the default is defined by Node.js).
exec :: String -> Effect ChildProcess
exec command = coerce $ UnsafeCP.execOpts command { encoding: "buffer" }

-- | The combined output of a process called with `exec`.
type ExecResult =
  { stdout :: Buffer
  , stderr :: Buffer
  , error :: Maybe SystemError
  }

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `timeout` <number> Default: 0
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated and any output is truncated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `killSignal` <string> | <integer> Default: 'SIGTERM'
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
type ExecOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , timeout :: Maybe Milliseconds
  , maxBuffer :: Maybe Number
  , killSignal :: Maybe KillSignal
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , windowsHide :: Maybe Boolean
  , shell :: Maybe Shell
  }

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
  -> (ExecResult -> Effect Unit)
  -> Effect ChildProcess
exec' command buildOpts cb = coerce $ UnsafeCP.execOptsCb command opts \err sout serr ->
  cb { stdout: unsafeSOBToBuffer sout, stderr: unsafeSOBToBuffer serr, error: err }
  where
  opts =
    { encoding: "buffer"
    , cwd: fromMaybe undefined o.cwd
    , env: fromMaybe undefined o.env
    , timeout: fromMaybe undefined o.timeout
    , maxBuffer: fromMaybe undefined o.maxBuffer
    , killSignal: fromMaybe undefined o.killSignal
    , uid: fromMaybe undefined o.uid
    , gid: fromMaybe undefined o.gid
    , windowsHide: fromMaybe undefined o.windowsHide
    , shell: fromMaybe undefined o.shell
    }
  o = buildOpts
    { cwd: Nothing
    , env: Nothing
    , timeout: Nothing
    , maxBuffer: Nothing
    , killSignal: Nothing
    , uid: Nothing
    , gid: Nothing
    , windowsHide: Nothing
    , shell: Nothing
    }

-- | Generally identical to `execFile`, with the exception that
-- | the method will not return until the child process has fully closed.
-- | Returns: The `stdout` from the command.
execFileSync
  :: String
  -> Array String
  -> Effect Buffer
execFileSync file args =
  map unsafeSOBToBuffer $ UnsafeCP.execFileSync' file args { encoding: "buffer" }

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `input` <string> | <Buffer> | <TypedArray> | <DataView> The value which will be passed as stdin to the spawned process. Supplying this value will override stdio[0].
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `timeout` <number> In milliseconds the maximum amount of time the process is allowed to run. Default: undefined.
-- | - `killSignal` <string> | <integer> The signal value to be used when the spawned process will be killed. Default: 'SIGTERM'.
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
type ExecFileSyncOptions =
  { cwd :: Maybe String
  , input :: Maybe Buffer
  , appendStdio :: Maybe (Array StdIO)
  , env :: Maybe (Object String)
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , timeout :: Maybe Milliseconds
  , killSignal :: Maybe KillSignal
  , maxBuffer :: Maybe Number
  , windowsHide :: Maybe Boolean
  , shell :: Maybe Shell
  }

execFileSync'
  :: String
  -> Array String
  -> (ExecFileSyncOptions -> ExecFileSyncOptions)
  -> Effect Buffer
execFileSync' file args buildOpts =
  map unsafeSOBToBuffer $ UnsafeCP.execFileSync' file args opts
  where
  opts =
    { stdio: [ pipe, pipe, pipe ] <> fromMaybe [] o.appendStdio
    , encoding: "buffer"
    , cwd: fromMaybe undefined o.cwd
    , input: fromMaybe undefined o.input
    , env: fromMaybe undefined o.env
    , uid: fromMaybe undefined o.uid
    , gid: fromMaybe undefined o.gid
    , timeout: fromMaybe undefined o.timeout
    , killSignal: fromMaybe undefined o.killSignal
    , maxBuffer: fromMaybe undefined o.maxBuffer
    , windowsHide: fromMaybe undefined o.windowsHide
    , shell: fromMaybe undefined o.shell
    }
  o = buildOpts
    { cwd: Nothing
    , input: Nothing
    , appendStdio: Nothing
    , env: Nothing
    , uid: Nothing
    , gid: Nothing
    , timeout: Nothing
    , killSignal: Nothing
    , maxBuffer: Nothing
    , windowsHide: Nothing
    , shell: Nothing
    }

-- | Like `exec`, except instead of using a shell, it passes the arguments
-- | directly to the specified command.
execFile
  :: String
  -> Array String
  -> Effect ChildProcess
execFile cmd args = coerce $ UnsafeCP.execFileOpts cmd args { encoding: "buffer" }

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `timeout` <number> Default: 0
-- | - `maxBuffer` <number> Largest amount of data in bytes allowed on stdout or stderr. If exceeded, the child process is terminated and any output is truncated. See caveat at maxBuffer and Unicode. Default: 1024 * 1024.
-- | - `killSignal` <string> | <integer> Default: 'SIGTERM'
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `windowsHide` <boolean> Hide the subprocess console window that would normally be created on Windows systems. Default: false.
-- | - `windowsVerbatimArguments` <boolean> No quoting or escaping of arguments is done on Windows. Ignored on Unix. Default: false.
-- | - `shell` <boolean> | <string> If true, runs command inside of a shell. Uses '/bin/sh' on Unix, and process.env.ComSpec on Windows. A different shell can be specified as a string. See Shell requirements and Default Windows shell. Default: false (no shell).
type ExecFileOptions =
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , timeout :: Maybe Milliseconds
  , maxBuffer :: Maybe Number
  , killSignal :: Maybe KillSignal
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  , windowsHide :: Maybe Boolean
  , windowsVerbatimArguments :: Maybe Boolean
  , shell :: Maybe Shell
  }

execFile'
  :: String
  -> Array String
  -> (ExecFileOptions -> ExecFileOptions)
  -> (ExecResult -> Effect Unit)
  -> Effect ChildProcess
execFile' cmd args buildOpts cb = coerce $ UnsafeCP.execFileOptsCb cmd args opts \err sout serr ->
  cb { stdout: unsafeSOBToBuffer sout, stderr: unsafeSOBToBuffer serr, error: err }
  where
  opts =
    { cwd: fromMaybe undefined o.cwd
    , env: fromMaybe undefined o.env
    , encoding: "buffer"
    , timeout: fromMaybe undefined o.timeout
    , maxBuffer: fromMaybe undefined o.maxBuffer
    , killSignal: fromMaybe undefined o.killSignal
    , uid: fromMaybe undefined o.uid
    , gid: fromMaybe undefined o.gid
    , windowsHide: fromMaybe undefined o.windowsHide
    , windowsVerbatimArguments: fromMaybe undefined o.windowsVerbatimArguments
    , shell: fromMaybe undefined o.shell
    }
  o = buildOpts
    { cwd: Nothing
    , env: Nothing
    , timeout: Nothing
    , maxBuffer: Nothing
    , killSignal: Nothing
    , uid: Nothing
    , gid: Nothing
    , windowsHide: Nothing
    , windowsVerbatimArguments: Nothing
    , shell: Nothing
    }

-- | A special case of `spawn` for creating Node.js child processes. The first
-- | argument is the module to be run, and the second is the argv (command line
-- | arguments).
fork
  :: String
  -> Array String
  -> Effect ChildProcess
fork modulePath args = coerce $ UnsafeCP.fork' modulePath args { stdio: [ pipe, pipe, pipe, ipc ] }

-- | - `cwd` <string> | <URL> Current working directory of the child process.
-- | - `detached` <boolean> Prepare child to run independently of its parent process. Specific behavior depends on the platform, see options.detached).
-- | - `env` <Object> Environment key-value pairs. Default: process.env.
-- | - `execPath` <string> Executable used to create the child process.
-- | - `execArgv` <string[]> List of string arguments passed to the executable. Default: process.execArgv.
-- | - `gid` <number> Sets the group identity of the process (see setgid(2)).
-- | - `serialization` <string> Specify the kind of serialization used for sending messages between processes. Possible values are 'json' and 'advanced'. See Advanced serialization for more details. Default: 'json'.
-- | - `signal` <AbortSignal> Allows closing the child process using an AbortSignal.
-- | - `killSignal` <string> | <integer> The signal value to be used when the spawned process will be killed by timeout or abort signal. Default: 'SIGTERM'.
-- | - `silent` <boolean> If true, stdin, stdout, and stderr of the child will be piped to the parent, otherwise they will be inherited from the parent, see the 'pipe' and 'inherit' options for child_process.spawn()'s stdio for more details. Default: false.
-- | - `uid` <number> Sets the user identity of the process (see setuid(2)).
-- | - `windowsVerbatimArguments` <boolean> No quoting or escaping of arguments is done on Windows. Ignored on Unix. Default: false.
-- | - `timeout` <number> In milliseconds the maximum amount of time the process is allowed to run. Default: undefined.
type ForkOptions =
  { cwd :: Maybe String
  , detached :: Maybe Boolean
  , appendStdio :: Maybe (Array StdIO)
  , env :: Maybe (Object String)
  , execPath :: Maybe String
  , execArgv :: Maybe (Array String)
  , gid :: Maybe Gid
  , serialization :: Maybe String
  , killSignal :: Maybe KillSignal
  , silent :: Maybe Boolean
  , uid :: Maybe Uid
  , windowsVerbatimArguments :: Maybe Boolean
  , timeout :: Maybe Milliseconds
  }

fork'
  :: String
  -> Array String
  -> (ForkOptions -> ForkOptions)
  -> Effect ChildProcess
fork' modulePath args buildOpts = coerce $ UnsafeCP.fork' modulePath args opts
  where
  opts =
    { stdio: [ pipe, pipe, pipe, ipc ] <> fromMaybe [] o.appendStdio
    , cwd: fromMaybe undefined o.cwd
    , detached: fromMaybe undefined o.detached
    , env: fromMaybe undefined o.env
    , execPath: fromMaybe undefined o.execPath
    , execArgv: fromMaybe undefined o.execArgv
    , gid: fromMaybe undefined o.gid
    , serialization: fromMaybe undefined o.serialization
    , killSignal: fromMaybe undefined o.killSignal
    , silent: fromMaybe undefined o.silent
    , uid: fromMaybe undefined o.uid
    , windowsVerbatimArguments: fromMaybe undefined o.windowsVerbatimArguments
    , timeout: fromMaybe undefined o.timeout
    }
  o = buildOpts
    { cwd: Nothing
    , detached: Nothing
    , appendStdio: Nothing
    , env: Nothing
    , execPath: Nothing
    , execArgv: Nothing
    , gid: Nothing
    , serialization: Nothing
    , killSignal: Nothing
    , silent: Nothing
    , uid: Nothing
    , windowsVerbatimArguments: Nothing
    , timeout: Nothing
    }

-- | Send messages to the (`nodejs`) child process.
-- |
-- | See the [node documentation](https://nodejs.org/api/child_process.html#child_process_subprocess_send_message_sendhandle_options_callback)
-- | for in-depth documentation.
send
  :: forall props
   . { | props }
  -> Maybe Handle
  -> ChildProcess
  -> Effect Boolean
send msg handle cp = UnsafeCP.unsafeSend msg (toNullable handle) (coerce cp)

-- | - `keepAlive` <boolean> A value that can be used when passing instances of `net.Socket` as the `Handle`. When true, the socket is kept open in the sending process. Default: false.
type SendOptions =
  { keepAlive :: Maybe Boolean
  }

send'
  :: forall props
   . { | props }
  -> Maybe Handle
  -> (SendOptions -> SendOptions)
  -> (Maybe Error -> Effect Unit)
  -> ChildProcess
  -> Effect Boolean
send' msg handle buildOpts cb cp =
  UnsafeCP.unsafeSendOptsCb msg (toNullable handle) opts cb (coerce cp)
  where
  opts =
    { keepAlive: fromMaybe undefined o.keepAlive }
  o = buildOpts
    { keepAlive: Nothing
    }

-- Unfortunately, there's not  be a better way...
foreign import undefined :: forall a. a
