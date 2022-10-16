module Test.Main (main) where

import Prelude (Unit, ($))
import Control.Apply ((*>))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Node.ChildProcess as ChildProcess
import Test.Node.ChildProcess.Aff as ChildProcess.Aff

main :: Effect Unit
main = launchAff_ $ ChildProcess.test *> ChildProcess.Aff.test
