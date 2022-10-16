module Test.Main (main) where

import Prelude (Unit)
import Effect (Effect)
import Test.Node.ChildProcess as ChildProcess

main :: Effect Unit
main = ChildProcess.test
