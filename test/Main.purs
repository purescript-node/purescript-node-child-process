module Test.Main where

import Prelude
import Control.Bind

import Control.Monad.Eff.Console

import Node.Encoding (Encoding(UTF8))
import Node.Buffer as Buffer
import Node.ChildProcess
import Node.ChildProcess.Signal
import Node.Stream (onData)

main = do
  ls <- spawn "ls" ["-la"] defaultSpawnOptions
  onClose ls \code sig ->
      log $ "ls exited with code: " ++ (show code) ++ "\nfrom signal: " ++ (show sig)
  onData (stdout ls) (Buffer.toString UTF8 >=> log)
  kill sigterm ls
  log "Killed."
