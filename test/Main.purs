module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Posix.Signal (Signal(..))
import Node.Buffer as Buffer
import Node.ChildProcess (CHILD_PROCESS, Exit(..), defaultExecOptions, exec, onError,
                          defaultSpawnOptions, spawn, stdout, onExit, kill)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (onData)

main :: forall eff. Eff
                      ( cp :: CHILD_PROCESS
                      , console :: CONSOLE
                      , err :: EXCEPTION
                      , buffer :: Buffer.BUFFER
                      | eff
                      ) Unit
main = do
  log "spawns processes ok"
  spawnLs

  log "emits an error if executable does not exist"
  nonExistentExecutable $ do
    log "nonexistent executable: all good."

  log "doesn't perform effects too early"
  spawn "ls" ["-la"] defaultSpawnOptions >>= \ls -> do
    let unused = kill SIGTERM ls
    onExit ls \exit ->
      case exit of
        Normally 0 ->
          log "All good!"
        _ -> do
          log ("Bad exit: expected `Normally 0`, got: " <> show exit)

  log "kills processes"
  spawn "ls" ["-la"] defaultSpawnOptions >>= \ls -> do
    kill SIGTERM ls
    onExit ls \exit ->
      case exit of
        BySignal SIGTERM ->
          log "All good!"
        _ -> do
          log ("Bad exit: expected `BySignal SIGTERM`, got: " <> show exit)

  log "exec"
  execLs

spawnLs :: forall eff. Eff ( cp :: CHILD_PROCESS
                           , console :: CONSOLE
                           , err :: EXCEPTION
                           , buffer :: Buffer.BUFFER
                           | eff
                           ) Unit
spawnLs = do
  ls <- spawn "ls" ["-la"] defaultSpawnOptions
  onExit ls \exit ->
      log $ "ls exited: " <> show exit
  onData (stdout ls) (Buffer.toString UTF8 >=> log)

nonExistentExecutable
  :: forall eff
   . Eff ( console :: CONSOLE
         , cp :: CHILD_PROCESS
         | eff
         ) Unit
  -> Eff ( cp :: CHILD_PROCESS
         , console :: CONSOLE
         | eff
         ) Unit
nonExistentExecutable done = do
  ch <- spawn "this-does-not-exist" [] defaultSpawnOptions
  onError ch (\err -> log err.code *> done)

execLs :: forall eff. Eff ( cp :: CHILD_PROCESS
                          , console :: CONSOLE
                          , buffer :: Buffer.BUFFER
                          | eff
                          ) Unit
execLs = do
  exec "ls >&2" defaultExecOptions \r ->
    log "redirected to stderr:" *> (Buffer.toString UTF8 r.stderr >>= log)
