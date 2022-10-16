module Test.Node.ChildProcess.Aff (test) where

import Prelude (($), Unit)
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Class (liftEffect)

test :: Aff Unit
test = liftEffect $ log "running Test.Node.ChildProcess.Aff test"
