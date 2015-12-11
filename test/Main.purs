module Test.Main where

import Prelude
import Control.Apply
import Control.Bind

import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe

import Node.Encoding (Encoding(UTF8))
import Node.Buffer as Buffer
import Node.ChildProcess
import Node.ChildProcess.Signal
import Node.Stream (onData)

main = do
  log "spawns processes ok"
  spawnLs

  log "emits an error if executable does not exist"
  nonExistentExecutable $ do
    log "all good."

spawnLs = do
  ls <- spawn "ls" ["-la"] defaultSpawnOptions
  onExit ls \exit ->
      log $ "ls exited: " <> show exit
  onData (stdout ls) (Buffer.toString UTF8 >=> log)

nonExistentExecutable done = do
  ch <- spawn "this-does-not-exist" [] defaultSpawnOptions
  onError ch (\err -> logAny err *> done)
