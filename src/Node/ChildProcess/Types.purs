module Node.ChildProcess.Types
  ( UnsafeChildProcess
  , Handle
  , StdIO
  , pipe
  , ignore
  , overlapped
  , ipc
  , inherit
  , shareStream
  , fileDescriptor
  , fileDescriptor'
  , defaultStdIO
  , KillSignal
  , intSignal
  , stringSignal
  , fromKillSignal
  , Shell
  , enableShell
  , customShell
  , StringOrBuffer
  , Exit(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Nullable (Nullable, null)
import Node.FS (FileDescriptor)
import Node.Stream (Stream)
import Unsafe.Coerce (unsafeCoerce)

-- | A child process with no guarantees about whether or not
-- | properties or methods (e.g. `stdin`, `send`) that depend on
-- | options or which function used to start the child process
-- | (e.g. `stdio`, `fork`) exist.
foreign import data UnsafeChildProcess :: Type

-- | A handle for inter-process communication (IPC).
foreign import data Handle :: Type

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

fromKillSignal :: KillSignal -> Either Int String
fromKillSignal sig = runFn3 fromKillSignalImpl Left Right sig

foreign import fromKillSignalImpl :: Fn3 (forall l r. l -> Either l r) (forall l r. r -> Either l r) (KillSignal) (Either Int String)

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
  | BySignal String

instance showExit :: Show Exit where
  show (Normally x) = "Normally " <> show x
  show (BySignal sig) = "BySignal " <> show sig
