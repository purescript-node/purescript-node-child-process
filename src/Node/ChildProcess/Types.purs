module Node.ChildProcess.Types where

import Prelude

import Data.Nullable (Nullable, null)
import Data.Posix.Signal (Signal)
import Node.EventEmitter (EventEmitter)
import Node.FS (FileDescriptor)
import Node.Stream (Stream)
import Unsafe.Coerce (unsafeCoerce)

-- | A child process with no guarantees about whether or not
-- | properties or methods (e.g. `stdin`, `send`) that depend on
-- | options or which function used to start the child process
-- | (e.g. `stdio`, `fork`) exist.
foreign import data UnsafeChildProcess :: Type

toEventEmitter :: UnsafeChildProcess -> EventEmitter
toEventEmitter = unsafeCoerce

-- | See https://nodejs.org/docs/latest-v18.x/api/child_process.html#optionsstdio
foreign import data StdIO :: Type

pipe :: StdIO
pipe = unsafeCoerce "pipe"

ignore :: StdIO
ignore = unsafeCoerce "ignore"

overlapped :: StdIO
overlapped = unsafeCoerce "overlapped"

ipc :: StdIO
ipc = unsafeCoerce "ipc"

inherit :: StdIO
inherit = unsafeCoerce "inherit"

shareStream :: forall r. Stream r -> StdIO
shareStream = unsafeCoerce

fileDescriptor :: Int -> StdIO
fileDescriptor = unsafeCoerce

fileDescriptor' :: FileDescriptor -> StdIO
fileDescriptor' = unsafeCoerce

defaultStdIO :: StdIO
defaultStdIO = unsafeCoerce (null :: Nullable String)

foreign import data KillSignal :: Type

intSignal :: Int -> KillSignal
intSignal = unsafeCoerce

stringSignal :: String -> KillSignal
stringSignal = unsafeCoerce

foreign import data Shell :: Type

enableShell :: Shell
enableShell = unsafeCoerce true

customShell :: String -> Shell
customShell = unsafeCoerce

-- | Indicates value is either a String or a Buffer depending on
-- | what options were used.
foreign import data StringOrBuffer :: Type

-- | Specifies how a child process exited; normally (with an exit code), or
-- | due to a signal.
data Exit
  = Normally Int
  | BySignal Signal

instance showExit :: Show Exit where
  show (Normally x) = "Normally " <> show x
  show (BySignal sig) = "BySignal " <> show sig
