module Test.Node.ChildProcess where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Data.Either (Either(Left, Right))
import Control.Parallel (parSequence_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Node.Buffer as Buffer
import Node.ChildProcess (Exit(..), defaultExecOptions, exec, defaultExecSyncOptions, execSync, onError, defaultSpawnOptions, spawn, stdout, onExit, kill)
import Node.Encoding (Encoding(UTF8))
import Node.Encoding as NE
import Node.Stream (onData)
import Test.Internal (makeNonCancelerAff)

test :: Aff Unit
test = do
    liftEffect $ log "running Test.Node.ChildProcess"
    parSequence_ [
      makeNonCancelerAff spawnLs,
      makeNonCancelerAff nonExistentExecutable, 
      makeNonCancelerAff spawnLsErr,
      makeNonCancelerAff spawnLsKills,
      makeNonCancelerAff execLs
    ]

spawnLs :: forall a. (Either a Unit -> Effect Unit) -> Effect Unit
spawnLs cb = do
  log "spawns processes ok"
  ls <- spawn "ls" ["-la"] defaultSpawnOptions
  onExit ls \exit -> do
     log $ "ls exited: " <> show exit
     cb $ Right unit
  onData (stdout ls) (Buffer.toString UTF8 >=> log)

nonExistentExecutable :: (Either Error Unit -> Effect Unit) -> Effect Unit
nonExistentExecutable cb = do
  log "emits an error if executable does not exist"
  ch <- spawn "this-does-not-exist" [] defaultSpawnOptions
  onError ch (\err -> log err.code *> cb (Left $ error err.code))
  log "nonexistent executable: all good."
  cb (Right unit)

spawnLsErr :: (Either Error Unit -> Effect Unit) -> Effect Unit
spawnLsErr cb = do
    log "doesn't perform effects too early"
    spawn "ls" ["-la"] defaultSpawnOptions >>= \ls -> do
      let _ = kill SIGTERM ls
      onExit ls \exit ->
        case exit of
          Normally 0 -> do
            log "All good!"
            cb $ Right unit
          _ -> do
            let e = error ("Bad exit: expected `Normally 0`, got: " <> show exit)
            log $ show e
            cb $ Left e

spawnLsKills :: (Either Error Unit -> Effect Unit) -> Effect Unit
spawnLsKills cb = do
    log "kills processes"
    spawn "ls" ["-la"] defaultSpawnOptions >>= \ls -> do
      _ <- kill SIGTERM ls
      onExit ls \exit ->
        case exit of
          BySignal SIGTERM -> do
            log "All good!"
            cb $ Right unit
          _ -> do
            let e = error ("Bad exit: expected `BySignal SIGTERM`, got: " <> show exit)
            log $ show e
            cb $ Left e

execLs :: (Either Error Unit -> Effect Unit) -> Effect Unit
execLs cb = do
    log "exec"
    -- returned ChildProcess is ignored here
    _ <- exec "ls >&2" defaultExecOptions \r -> do
      log "redirected to stderr:"
      (Buffer.toString UTF8 r.stderr >>= log)
      cb $ Right unit
    pure unit

execSyncEcho :: String -> Effect Unit
execSyncEcho str = do
  resBuf <- execSync "cat" (defaultExecSyncOptions {input = Just str})
  res <- Buffer.toString NE.UTF8 resBuf
  log res
