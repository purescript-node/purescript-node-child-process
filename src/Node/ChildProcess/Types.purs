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
  , fromKillSignal'
  , Shell
  , enableShell
  , customShell
  , StringOrBuffer
  , Exit(..)
  ) where

import Prelude

import Data.Either (Either(..), either)
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

instance Eq KillSignal where
  eq a b = a # fromKillSignal'
    ( \i -> b # fromKillSignal'
        (\b' -> i == b')
        (const false)
    )
    ( \s -> b # fromKillSignal'
        (const false)
        (\b' -> s == b')
    )

instance Show KillSignal where
  show = showKillSignal

foreign import showKillSignal :: KillSignal -> String

intSignal :: Int -> KillSignal
intSignal = unsafeCoerce

stringSignal :: String -> KillSignal
stringSignal = unsafeCoerce

fromKillSignal :: KillSignal -> Either Int String
fromKillSignal sig = fromKillSignal' Left Right sig

fromKillSignal' :: forall r. (Int -> r) -> (String -> r) -> KillSignal -> r
fromKillSignal' fromInt fromStr sig = runFn3 fromKillSignalImpl fromInt fromStr sig

foreign import fromKillSignalImpl :: forall r. Fn3 (Int -> r) (String -> r) (KillSignal) r

foreign import data Shell :: Type

instance Show Shell where
  show = showShell

foreign import showShell :: Shell -> String

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
  | BySignal KillSignal

instance showExit :: Show Exit where
  show (Normally x) = "Normally " <> show x
  show (BySignal sig) = "BySignal " <> (either show show $ fromKillSignal sig)
