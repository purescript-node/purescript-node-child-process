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
import Node.Stream (Read, Readable, Stream, Writable, Write, Duplex)
import Prim.Boolean (True, False)
import Type.Proxy (Proxy)
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
-- | * `ShareStream`: Connect the supplied stream to the corresponding file
-- |    descriptor in the child.
-- | * `ShareFD`: Connect the supplied file descriptor (which should be open
-- |   in the parent) to the corresponding file descriptor in the child.
-- | * `Ipc`: Enables `send`/`disconnect`/`onDisconnect`/`onMessage`. Only 1 `Ipc` per `stdio` file descriptor.
-- |
-- | Note: `"inherit"` is not supported. When used in any other `stdio` slot besides
-- | index `0`, `1`, or `2`, it is the same as `"ignore"`.
-- | Since it's used to inherit the parent process' `stdin`/`stdout`/`stderr`,
-- | this functionality is built into the `toStdIOOption` function
foreign import data StdIO :: Row Type -> Type

-- | Creates a pipe between the child and parent process, which can
-- |   then be accessed as a `Stream` via the `stdin`, `stdout`, or `stderr`
-- |   functions.
pipe :: forall pipe r. { usePipe :: Stream pipe | r } -> Stream pipe
pipe = _.usePipe

pipeRead :: StdIO (read :: Read)
pipeRead = unsafeCoerce "pipe"

pipeWrite :: StdIO (write :: Write)
pipeWrite = unsafeCoerce "pipe"

-- | Creates a pipe between the child and parent process, which can
-- |   then be accessed as a `Stream` via the `stdin`, `stdout`, or `stderr`
-- |   functions.
-- | For Windows, also enables the `FILE_FLAG_OVERLAPPED` on the handle. See
-- |    https://learn.microsoft.com/en-us/windows/win32/fileio/synchronous-and-asynchronous-i-o
overlapped :: forall overlapped r. { useOverlapped :: Stream overlapped | r } -> Stream overlapped
overlapped { useOverlapped } = useOverlapped

overlappedRead :: StdIO (read :: Read)
overlappedRead = unsafeCoerce "overlapped"

overlappedWrite :: StdIO (write :: Write)
overlappedWrite = unsafeCoerce "overlapped"

inherit :: forall inherit r. { useInherit :: Stream inherit | r } -> Stream inherit
inherit { useInherit } = useInherit

inheritRead :: StdIO (read :: Read)
inheritRead = unsafeCoerce "inherit"

inheritWrite :: StdIO (write :: Write)
inheritWrite = unsafeCoerce "inherit"

type IgnoreMsg = "'ignore' was used for this IO slot, so this stream on the child process doesn't exist."

ignore :: forall a. a -> StdIO (read :: Proxy IgnoreMsg, write :: Proxy IgnoreMsg)
ignore _ = unsafeCoerce "ignore"

fileDescriptor :: forall a. FileDescriptor -> a -> StdIO (read :: Read, write :: Write)
fileDescriptor fd _ = unsafeCoerce fd

shareReadStream :: forall a. Readable () -> a -> StdIO (read :: Read)
shareReadStream s _ = unsafeCoerce s

shareWriteStream :: forall a. Writable () -> a -> StdIO (write :: Write)
shareWriteStream s _ = unsafeCoerce s

shareDuplexStream :: forall a. Duplex -> a -> StdIO (read :: Read, write :: Write)
shareDuplexStream s _ = unsafeCoerce s

newtype IpcOption :: Boolean -> Type
newtype IpcOption used = IpcOption Boolean

useIpc :: IpcOption True
useIpc = IpcOption true

noIpc :: IpcOption False
noIpc = IpcOption false

foreign import data StdIoOption :: Row Type -> Row Type -> Row Type -> Boolean -> Type

type StdInChoices =
  { usePipe :: StdIO (write :: Write)
  , useOverlapped :: StdIO (write :: Write)
  , useInherit :: StdIO (read :: Read)
  }

type StdOutErrChoices =
  { usePipe :: StdIO (read :: Read)
  , useOverlapped :: StdIO (read :: Read)
  , useInherit :: StdIO (write :: Write)
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
  }

-- Using `pipe` or `overlapped` means we can read the child process' output, so we use `read` here.
-- But using `inherit` means we're reusing the process' `stdout`/`stderr`, which are `Writeable ()`, so we us `write` there.
choicesStdOutErr :: StdOutErrChoices
choicesStdOutErr =
  { usePipe: pipeRead
  , useOverlapped: overlappedRead
  , useInherit: inheritWrite
  }

-- | Intended usage is below. 
-- | ```
-- | `spawn' "cmd" [ "args" ] (_ 
-- |    { stdio = Just $ 
-- |        toStdIoOption (_ 
-- |            { stdin = Just inherit
-- |            , stdout = Just $ shareStream someWriteStream
-- |            , stderr = Just ignore
-- |            }
-- |        )
-- |    })
-- | ```
-- |
-- | The verbosity of the `ConfigureStdIoOptionsFn` type is needed
-- | to make `stdin`/`stdout`/`stderr` type safe.
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
