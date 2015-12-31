module Test.Main where

import Prelude
import Control.Apply
import Control.Bind

import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe

import Data.Posix.Signal (Signal(..))
import Node.Encoding (Encoding(UTF8))
import Node.Buffer as Buffer
import Node.ChildProcess
import Node.Stream (onData)

main = do
  log "spawns processes ok"
  spawnLs

  log "emits an error if executable does not exist"
  nonExistentExecutable $ do
    log "nonexistent executable: all good."

  log "doesn't perform effects too early"
  ls <- spawn "ls" ["-la"] defaultSpawnOptions
  let unused = kill SIGTERM ls
  onExit ls \exit ->
    case exit of
      Normally 0 ->
        log "All good!"
      _ -> do
        log ("Bad exit: expected `Normally 0`, got: " <> show exit)

  log "kills processes"
  ls <- spawn "ls" ["-la"] defaultSpawnOptions
  kill SIGTERM ls
  onExit ls \exit ->
    case exit of
      BySignal SIGTERM ->
        log "All good!"
      _ -> do
        log ("Bad exit: expected `BySignal SIGTERM`, got: " <> show exit)

  log "exec"
  execLs

spawnLs = do
  ls <- spawn "ls" ["-la"] defaultSpawnOptions
  onExit ls \exit ->
      log $ "ls exited: " <> show exit
  onData (stdout ls) (Buffer.toString UTF8 >=> log)

nonExistentExecutable done = do
  ch <- spawn "this-does-not-exist" [] defaultSpawnOptions
  onError ch (\err -> logAny err *> done)

execLs = do
  exec "ls >&2" defaultExecOptions \r ->
    log "redirected to stderr:" *> (Buffer.toString UTF8 r.stderr >>= log)
