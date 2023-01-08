-- | See https://nodejs.org/api/child_process.html#optionsstdio
-- | for full context.
module Node.ChildProcess.StdIO
  ( StdIO
  , pipe
  , overlapped
  , ignore
  , inherit
  , shareReadStream
  , shareWriteStream
  , shareDuplexStream
  , fileDescriptor
  , IpcOption
  , useIpc
  , StdIoOption
  , toStdIoOption
  , StdInChoices
  , StdOutErrChoices
  , ConfigureStdIoOptionsFn
  ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Foreign (Foreign, unsafeToForeign)
import Node.FS (FileDescriptor)
import Node.Stream (Duplex, Readable, Stream, Writable)
import Prim.Boolean (True, False)
import Unsafe.Coerce (unsafeCoerce)

-- | Behaviour for standard IO streams (eg, standard input, standard output) of
-- | a child process.
-- |
-- | * `Pipe`: creates a pipe between the child and parent process, which can
-- |   then be accessed as a `Stream` via the `stdin`, `stdout`, or `stderr`
-- |   functions.
-- | * `Overlapped`: Same as `pipe` on `non-Windows` OSes. For Windows, see
-- |    https://learn.microsoft.com/en-us/windows/win32/fileio/synchronous-and-asynchronous-i-o
-- | * `Ignore`: ignore this stream. This will cause Node to open /dev/null and
-- |   connect it to the stream.
-- | * `Inherit`: share the same stream as the parent process. When used
-- |   in any other place besides indices `0`, `1`, and `2` (i.e. `stdin`/`stdout`/`stderr`)
-- |   it's the same as `ignore`. Thus, this functionality is built into the `toStdIoOption` function
-- | * `ShareStream`: Connect the supplied stream to the corresponding file
-- |    descriptor in the child.
-- | * `ShareFD`: Connect the supplied file descriptor (which should be open
-- |   in the parent) to the corresponding file descriptor in the child.
foreign import data StdIO :: Type -> Type

-- | Creates a pipe between the child and parent process, which can
-- |   then be accessed as a `Stream` via the `stdin`, `stdout`, or `stderr`
-- |   functions.
pipe :: forall pipe r. { usePipe :: Stream pipe | r } -> Stream pipe
pipe = _.usePipe

pipeRead :: StdIO (Readable ())
pipeRead = unsafeCoerce "pipe"

pipeWrite :: StdIO (Writable ())
pipeWrite = unsafeCoerce "pipe"

-- | Creates a pipe between the child and parent process, which can
-- |   then be accessed as a `Stream` via the `stdin`, `stdout`, or `stderr`
-- |   functions.
-- | For Windows, also enables the `FILE_FLAG_OVERLAPPED` on the handle. See
-- |    https://learn.microsoft.com/en-us/windows/win32/fileio/synchronous-and-asynchronous-i-o
overlapped :: forall overlapped r. { useOverlapped :: Stream overlapped | r } -> Stream overlapped
overlapped { useOverlapped } = useOverlapped

overlappedRead :: StdIO (Readable ())
overlappedRead = unsafeCoerce "overlapped"

overlappedWrite :: StdIO (Writable ())
overlappedWrite = unsafeCoerce "overlapped"

inherit :: forall inherit r. { useInherit :: Stream inherit | r } -> Stream inherit
inherit { useInherit } = useInherit

inheritRead :: StdIO (Readable ())
inheritRead = unsafeCoerce "inherit"

inheritWrite :: StdIO (Writable ())
inheritWrite = unsafeCoerce "inherit"

type IgnoreMsg = "'ignore' was used for this IO slot, so this stream on the child process doesn't exist."

ignore :: forall a. a -> StdIO Void
ignore _ = unsafeCoerce "ignore"

fileDescriptor :: forall a. FileDescriptor -> a -> StdIO Duplex
fileDescriptor fd _ = unsafeCoerce fd

shareReadStream :: forall r. Readable () -> { useReadStream :: Readable () -> StdIO (Readable ()) | r } -> StdIO (Readable ())
shareReadStream s { useReadStream } = useReadStream s

shareWriteStream :: forall r. Writable () -> { useWriteStream :: Writable () -> StdIO (Writable ()) | r } -> StdIO (Writable ())
shareWriteStream s { useWriteStream } = useWriteStream s

shareDuplexStream :: forall r. Duplex -> { useDuplexStream :: Duplex -> StdIO Duplex | r } -> StdIO Duplex
shareDuplexStream s { useDuplexStream } = useDuplexStream s

-- | `Ipc`: Enables `send`/`disconnect`/`onDisconnect`/`onMessage`. Only 1 `Ipc` per `stdio` file descriptor.
-- | Similar to `inherit`, this value is handled specially in `toStdIoOption`.
newtype IpcOption :: Boolean -> Type
newtype IpcOption used = IpcOption Boolean

useIpc :: IpcOption True
useIpc = IpcOption true

noIpc :: IpcOption False
noIpc = IpcOption false

foreign import data StdIoOption :: Type -> Type -> Type -> Boolean -> Type

type StdInChoices =
  { usePipe :: StdIO (Writable ())
  , useOverlapped :: StdIO (Writable ())
  , useInherit :: StdIO (Readable ())
  , useReadStream :: Readable () -> StdIO (Readable ())
  , useDuplexStream :: Duplex -> StdIO Duplex
  }

type StdOutErrChoices =
  { usePipe :: StdIO (Readable ())
  , useOverlapped :: StdIO (Readable ())
  , useInherit :: StdIO (Writable ())
  , useWriteStream :: Writable () -> StdIO (Writable ())
  , useDuplexStream :: Duplex -> StdIO Duplex
  }

-- | Ignore this type and just look at `toStdIoOption`
type ConfigureStdIoOptionsFn stdInUsed stdOutUsed stdErrUsed ipcUsed =
  { stdin :: Maybe (StdInChoices -> StdIO stdInUsed)
  , stdout :: Maybe (StdOutErrChoices -> StdIO stdOutUsed)
  , stderr :: Maybe (StdOutErrChoices -> StdIO stdErrUsed)
  , ipc :: Maybe (IpcOption False -> IpcOption ipcUsed)
  , extra :: Array Foreign
  }
  -> { stdin :: Maybe (StdInChoices -> StdIO stdInUsed)
     , stdout :: Maybe (StdOutErrChoices -> StdIO stdOutUsed)
     , stderr :: Maybe (StdOutErrChoices -> StdIO stdErrUsed)
     , ipc :: Maybe (IpcOption False -> IpcOption ipcUsed)
     , extra :: Array Foreign
     }

-- Using `pipe` or `overlapped` means we can write to the child process' input, so we use `write` here.
-- But using `inherit` means we're reusing the process' `stdin`, which is `Readable ()`, so we us `read` there.
choicesStdin :: StdInChoices
choicesStdin =
  { usePipe: pipeWrite
  , useOverlapped: overlappedWrite
  , useInherit: inheritRead
  , useReadStream: unsafeCoerce
  , useDuplexStream: unsafeCoerce
  }

-- Using `pipe` or `overlapped` means we can read the child process' output, so we use `read` here.
-- But using `inherit` means we're reusing the process' `stdout`/`stderr`, which are `Writeable ()`, so we us `write` there.
choicesStdOutErr :: StdOutErrChoices
choicesStdOutErr =
  { usePipe: pipeRead
  , useOverlapped: overlappedRead
  , useInherit: inheritWrite
  , useWriteStream: unsafeCoerce
  , useDuplexStream: unsafeCoerce
  }

-- | Examples of intended usage are below. 
-- | ```
-- | -- use the defaults that Node provides
-- | spawn "cmd" [ "args" ]
-- | ```
-- | ```
-- | -- Override stdin/stdout/stderr with
-- | -- 1. `"inherit"` - make `stdin` share `process.stdin`
-- | -- 2. a write stream (e.g. to a file or HTTP response)
-- | -- 3. a file descriptor, enabling content to be read from / written to the file
-- | `spawn' "cmd" [ "args" ] (_ 
-- |    { stdio = Just $ 
-- |        toStdIoOption (_ 
-- |            { stdin = Just inherit
-- |            , stdout = Just $ shareWriteStream shareWriteStream
-- |            , stderr = Just $ fileDescriptor fd
-- |            }
-- |        )
-- |    })
-- | ```
-- |
-- | The verbosity of the `ConfigureStdIoOptionsFn` type is needed
-- | to make `stdin`/`stdout`/`stderr` type safe.
-- |
-- | This function makes the following guarantees about the resulting `stdio` array:
-- | - if the `stdio` option is `Nothing`, the defaults will be used
-- | - if the `stdio` option is `Just` and is overridden using this function, then:
-- |    - `stdio[0]` stores `null` or `childProcess.stdin` (depending on option)
-- |    - `stdio[1]` stores `null` or `childProcess.stdout` (depending on option)
-- |    - `stdio[2]` stores `null` or `childProcess.stderr` (depending on option)
-- |    - `stdio[3]` stores `null` or `childProcess.ipc` (depending on option)
-- |    - `stdio[4]` stores `null` or `extra[0]` (if supplied)
-- |    - `stdio[n+4]` stores `null` or `extra[n]` (if supplied)
toStdIoOption
  :: forall stdInUsed stdOutUsed stdErrUsed ipcUsed
   . ConfigureStdIoOptionsFn stdInUsed stdOutUsed stdErrUsed ipcUsed
  -> StdIoOption stdInUsed stdOutUsed stdErrUsed ipcUsed
toStdIoOption buildOptions = do
  let
    stdinOpt = maybe undefined stdIoToForeign $ map (\fn -> fn choicesStdin) options.stdin
    stdoutOpt = maybe undefined stdIoToForeign $ map (\fn -> fn choicesStdOutErr) options.stdout
    stderrOpt = maybe undefined stdIoToForeign $ map (\fn -> fn choicesStdOutErr) options.stderr
    ipcOpt
      | Just fn <- options.ipc
      , IpcOption true <- fn noIpc = unsafeToForeign "ipc"
      | otherwise = undefined

    toStdIO :: Array Foreign -> StdIoOption stdInUsed stdOutUsed stdErrUsed ipcUsed
    toStdIO = unsafeCoerce
  toStdIO $ [ stdinOpt, stdoutOpt, stderrOpt, ipcOpt ] <> options.extra
  where
  options = buildOptions defaults

  defaults =
    { stdin: Nothing
    , stdout: Nothing
    , stderr: Nothing
    , ipc: Nothing
    , extra: []
    }

  stdIoToForeign :: forall r. StdIO r -> Foreign
  stdIoToForeign = unsafeToForeign

foreign import undefined :: forall a. a
