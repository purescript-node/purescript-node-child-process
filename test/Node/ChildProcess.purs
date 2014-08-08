module Test.Node.ChildProcess where

  import Data.Function (mkFn2, runFn1)

  import Debug.Trace

  import Node.ChildProcess
  import Node.ChildProcess.Signal
  import Node.Events

  foreign import toString
    "function toString(x) {\
    \  return x == null ? 'null' : x.toString();\
    \}" :: forall a. a -> String

  main = do
    ChildProcess ls <- spawn "ls" ["-la"] defaultSpawnOptions
    on closeEvent (mkFn2 \code sig ->
        trace $ "ls exited with code: " ++
          (toString code) ++
          "\nfrom signal: " ++
          (toString sig))
      (ChildProcess ls)
    on (Event "data") (toString >>> trace) ls.stdout
    pure $ runFn1 ls.kill sigterm
