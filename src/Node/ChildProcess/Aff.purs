module Node.ChildProcess.Aff where

import Prelude

import Control.Parallel (parOneOf)
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Posix (Pid)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Ref as Ref
import Node.ChildProcess.Types (UnsafeChildProcess)
import Node.UnsafeChildProcess.Safe as CPSafe
import Node.ChildProcess (ChildProcess, toUnsafeChildProcess)
import Node.Errors.SystemError (SystemError)
import Node.EventEmitter (once)
import Partial.Unsafe (unsafePartial)

-- | Blocks until either a `spawn` or `error` event is fired.
-- | If a `spawn` event fired, child process was successfully started
-- | and the `pid` of the process can be obtained.
-- | If an `error` event fires, child process was not started successfully.
waitSpawned :: ChildProcess -> Aff (Either SystemError Pid)
waitSpawned = toUnsafeChildProcess >>> waitSpawned'

-- | Same as `waitSpawned` but works on `UnsafeChildProcess`
-- |
-- | Blocks until either a `spawn` or `error` event is fired.
-- | If a `spawn` event fired, child process was successfully started
-- | and the `pid` of the process can be obtained.
-- | If an `error` event fires, child process was not started successfully.
waitSpawned' :: UnsafeChildProcess -> Aff (Either SystemError Pid)
waitSpawned' cp = parOneOf [ pidOnSpawn, errored ]
  where
  pidOnSpawn = makeAff \done -> do
    ref <- Ref.new mempty
    removeListener <- cp # once CPSafe.spawnH do
      join $ Ref.read ref
      pid' <- CPSafe.pid cp
      done $ Right $ Right $ unsafePartial $ fromJust pid'
    Ref.write removeListener ref
    pure $ effectCanceler do
      removeListener

  errored = makeAff \done -> do
    ref <- Ref.new mempty
    removeListener <- cp # once CPSafe.errorH \sysErr -> do
      join $ Ref.read ref
      done $ Right $ Left sysErr
    Ref.write removeListener ref
    pure $ effectCanceler do
      removeListener
