-- | All API below is safe (i.e. does not crash if called) 
-- | and independent from options or which
-- | function was used to start the `ChildProcess`.
module Node.UnsafeChildProcess.Safe
  ( toEventEmitter
  , closeH
  , disconnectH
  , errorH
  , exitH
  , messageH
  , spawnH
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
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Posix (Pid)
import Data.Posix.Signal (Signal)
import Data.Posix.Signal as Signal
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)
import Node.ChildProcess.Types (Exit(..), Handle, KillSignal, StdIO, UnsafeChildProcess, intSignal, stringSignal)
import Node.Errors.SystemError (SystemError)
import Node.EventEmitter (EventEmitter, EventHandle(..))
import Node.EventEmitter.UtilTypes (EventHandle0, EventHandle1)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

toEventEmitter :: UnsafeChildProcess -> EventEmitter
toEventEmitter = unsafeCoerce

closeH :: EventHandle UnsafeChildProcess (Exit -> Effect Unit) (EffectFn2 (Nullable Int) (Nullable KillSignal) Unit)
closeH = EventHandle "close" \cb -> mkEffectFn2 \code signal ->
  case toMaybe code, toMaybe signal of
    Just c, _ -> cb $ Normally c
    _, Just s -> cb $ BySignal s
    _, _ -> unsafeCrashWith $ "Impossible. 'close' event did not get an exit code or kill signal: " <> show code <> "; " <> (unsafeCoerce signal)

disconnectH :: EventHandle0 UnsafeChildProcess
disconnectH = EventHandle "disconnect" identity

errorH :: EventHandle1 UnsafeChildProcess SystemError
errorH = EventHandle "error" mkEffectFn1

exitH :: EventHandle UnsafeChildProcess (Exit -> Effect Unit) (EffectFn2 (Nullable Int) (Nullable KillSignal) Unit)
exitH = EventHandle "exit" \cb -> mkEffectFn2 \code signal ->
  case toMaybe code, toMaybe signal of
    Just c, _ -> cb $ Normally c
    _, Just s -> cb $ BySignal s
    _, _ -> unsafeCrashWith $ "Impossible. 'exit' event did not get an exit code or kill signal: " <> show code <> "; " <> (unsafeCoerce signal)

messageH :: EventHandle UnsafeChildProcess (Foreign -> Maybe Handle -> Effect Unit) (EffectFn2 Foreign (Nullable Handle) Unit)
messageH = EventHandle "message" \cb -> mkEffectFn2 \a b -> cb a $ toMaybe b

spawnH :: EventHandle0 UnsafeChildProcess
spawnH = EventHandle "spawn" identity

-- | The process ID of a child process. Note that if the process has already
-- | exited, another process may have taken the same ID, so be careful!
pid :: UnsafeChildProcess -> Effect (Maybe Pid)
pid cp = map toMaybe $ runEffectFn1 pidImpl cp

foreign import pidImpl :: EffectFn1 (UnsafeChildProcess) (Nullable Pid)

-- | Note: this will not work if the user does not have permission to kill
-- | a `PID`. Uses `cp.kill(0)` underneath.
pidExists :: UnsafeChildProcess -> Effect Boolean
pidExists cp = kill' (intSignal 0) cp

-- | Indicates whether it is still possible to send and receive
-- | messages from the child process.
connected :: UnsafeChildProcess -> Effect Boolean
connected cp = runEffectFn1 connectedImpl cp

foreign import connectedImpl :: EffectFn1 (UnsafeChildProcess) (Boolean)

exitCode :: UnsafeChildProcess -> Effect (Maybe Int)
exitCode cp = map toMaybe $ runEffectFn1 exitCodeImpl cp

foreign import exitCodeImpl :: EffectFn1 (UnsafeChildProcess) (Nullable Int)

-- | Closes the IPC channel between parent and child.
disconnect :: UnsafeChildProcess -> Effect Unit
disconnect cp = runEffectFn1 disconnectImpl cp

foreign import disconnectImpl :: EffectFn1 (UnsafeChildProcess) (Unit)

kill :: UnsafeChildProcess -> Effect Boolean
kill cp = runEffectFn1 killImpl cp

foreign import killImpl :: EffectFn1 (UnsafeChildProcess) (Boolean)

kill' :: KillSignal -> UnsafeChildProcess -> Effect Boolean
kill' sig cp = runEffectFn2 killStrImpl cp sig

foreign import killStrImpl :: EffectFn2 (UnsafeChildProcess) (KillSignal) (Boolean)

-- | Send a signal to a child process. In the same way as the
-- | [unix kill(2) system call](https://linux.die.net/man/2/kill),
-- | sending a signal to a child process won't necessarily kill it.
-- |
-- | The resulting effects of this function depend on the process
-- | and the signal. They can vary from system to system.
-- | The child process might emit an `"error"` event if the signal
-- | could not be delivered.
killSignal :: Signal -> UnsafeChildProcess -> Effect Boolean
killSignal sig cp = kill' (stringSignal $ Signal.toString sig) cp

killed :: UnsafeChildProcess -> Effect Boolean
killed cp = runEffectFn1 killedImpl cp

foreign import killedImpl :: EffectFn1 (UnsafeChildProcess) (Boolean)

ref :: UnsafeChildProcess -> Effect Unit
ref cp = runEffectFn1 refImpl cp

foreign import refImpl :: EffectFn1 (UnsafeChildProcess) (Unit)

unref :: UnsafeChildProcess -> Effect Unit
unref cp = runEffectFn1 unrefImpl cp

foreign import unrefImpl :: EffectFn1 (UnsafeChildProcess) (Unit)

signalCode :: UnsafeChildProcess -> Effect (Maybe String)
signalCode cp = map toMaybe $ runEffectFn1 signalCodeImpl cp

foreign import signalCodeImpl :: EffectFn1 (UnsafeChildProcess) (Nullable String)

foreign import spawnArgs :: UnsafeChildProcess -> Array String

foreign import spawnFile :: UnsafeChildProcess -> String

foreign import stdio :: UnsafeChildProcess -> Array StdIO
