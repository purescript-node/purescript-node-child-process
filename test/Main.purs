module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer as Buffer
import Node.Buffer.Immutable as ImmutableBuffer
import Node.ChildProcess (exec, execSync', kill', onError, onExit, spawn, stdout)
import Node.Encoding (Encoding(UTF8))
import Node.Encoding as NE
import Node.Stream (onData)

main :: Effect Unit
main = do
  log "spawns processes ok"
  spawnLs

  log "emits an error if executable does not exist"
  nonExistentExecutable $ do
    log "nonexistent executable: all good."

  log "doesn't perform effects too early"
  spawn "ls" [ "-la" ] >>= \ls -> do
    let _ = kill' SIGTERM ls
    onExit ls \exit ->
      case exit.exitCode of
        Just 0 ->
          log "All good!"
        _ -> do
          log ("Bad exit: expected `Normally 0`, got: " <> show exit)

  log "kills processes"
  spawn "ls" [ "-la" ] >>= \ls -> do
    _ <- kill' SIGTERM ls
    onExit ls \exit ->
      case exit.signalCode >>= _.signal of
        Just SIGTERM ->
          log "All good!"
        _ -> do
          log ("Bad exit: expected `BySignal SIGTERM`, got: " <> show exit)

  log "exec"
  execLs

spawnLs :: Effect Unit
spawnLs = do
  ls <- spawn "ls" [ "-la" ]
  onExit ls \exit ->
    log $ "ls exited: " <> show exit
  for_ (stdout ls) \stream ->
    onData stream (Buffer.toString UTF8 >=> log)

nonExistentExecutable :: Effect Unit -> Effect Unit
nonExistentExecutable done = do
  ch <- spawn "this-does-not-exist" []
  onError ch (\err -> log err.code *> done)

execLs :: Effect Unit
execLs = do
  -- returned ChildProcess is ignored here
  _ <- exec "ls >&2" \r ->
    log "redirected to stderr:" *> (log $ ImmutableBuffer.toString UTF8 r.stderr)
  pure unit

execSyncEcho :: String -> Effect Unit
execSyncEcho str = do
  resBuf <- execSync' "cat" (_ { input = Just $ ImmutableBuffer.fromString str NE.UTF8 })
  log $ ImmutableBuffer.toString NE.UTF8 resBuf
