module Node.ChildProcess.Aff (spawn) where

import Prelude (($), discard, bind, pure)
import Data.Either (Either(Right))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Node.ChildProcess as Node.ChildProcess

-- | Spawn a child process. Note that, in the event that a child process could
-- | not be spawned (for example, if the executable was not found) this will
-- | throw an error.
spawn
  :: String
  -> Array String
  -> Node.ChildProcess.SpawnOptions
  -> Aff Node.ChildProcess.ChildProcess
spawn cmd args opts = makeAff (\cb -> do
  child <- Node.ChildProcess.spawn cmd args opts
  cb $ Right child
  pure nonCanceler
)
