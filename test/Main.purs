module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), defaultExecOptions, defaultExecSyncOptions, defaultSpawnOptions, errorH, exec, execSync, exitH, kill, spawn, stdout)
import Node.Encoding (Encoding(UTF8))
import Node.Encoding as NE
import Node.Errors.SystemError (code)
import Node.EventEmitter (on_)
import Node.Stream (dataH)

main :: Effect Unit
main = do
  log "spawns processes ok"
  spawnLs

  log "emits an error if executable does not exist"
  nonExistentExecutable $ do
    log "nonexistent executable: all good."

  log "doesn't perform effects too early"
  spawn "ls" [ "-la" ] defaultSpawnOptions >>= \ls -> do
    let _ = kill ls
    ls # on_ exitH \exit ->
      case exit of
        Normally 0 ->
          log "All good!"
        _ -> do
          log ("Bad exit: expected `Normally 0`, got: " <> show exit)

  log "kills processes"
  spawn "ls" [ "-la" ] defaultSpawnOptions >>= \ls -> do
    _ <- kill ls
    ls # on_ exitH \exit ->
      case exit of
        BySignal SIGTERM ->
          log "All good!"
        _ -> do
          log ("Bad exit: expected `BySignal SIGTERM`, got: " <> show exit)

  log "exec"
  execLs

spawnLs :: Effect Unit
spawnLs = do
  ls <- spawn "ls" [ "-la" ] defaultSpawnOptions
  ls # on_ exitH \exit ->
    log $ "ls exited: " <> show exit
  (stdout ls) # on_ dataH (Buffer.toString UTF8 >=> log)

nonExistentExecutable :: Effect Unit -> Effect Unit
nonExistentExecutable done = do
  ch <- spawn "this-does-not-exist" [] defaultSpawnOptions
  ch # on_ errorH \err ->
    log (code err) *> done

execLs :: Effect Unit
execLs = do
  -- returned ChildProcess is ignored here
  _ <- exec "ls >&2" defaultExecOptions \r ->
    log "redirected to stderr:" *> (Buffer.toString UTF8 r.stderr >>= log)
  pure unit

execSyncEcho :: String -> Effect Unit
execSyncEcho str = do
  resBuf <- execSync "cat" (defaultExecSyncOptions { input = Just str })
  res <- Buffer.toString NE.UTF8 resBuf
  log res
