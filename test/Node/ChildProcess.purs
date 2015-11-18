module Test.Node.ChildProcess where

  import Prelude

  import Data.Function (mkFn2, runFn1)

  import Control.Monad.Eff.Console

  import Node.ChildProcess
  import Node.ChildProcess.Signal
  import Node.Stream (onData)

  foreign import toString :: forall a. a -> String

  main = do
    ls <- spawn "ls" ["-la"] defaultSpawnOptions
    onClose ls \code sig ->
        log $ "ls exited with code: " ++ (toString code) ++ "\nfrom signal: " ++ (toString sig)
    onData ls.stdout (toString >>> log)
    pure $ runFn1 ls.kill sigterm
