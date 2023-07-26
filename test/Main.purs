module Test.Main where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Data.Posix.Signal as Signal
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw, throwException)
import Node.Buffer as Buffer
import Node.ChildProcess (exec', execSync', kill, spawn, stdin)
import Node.ChildProcess as CP
import Node.ChildProcess.Aff (waitSpawned)
import Node.ChildProcess.Types (Exit(..), fromKillSignal)
import Node.Encoding (Encoding(..))
import Node.Encoding as NE
import Node.EventEmitter (EventHandle, once, once_)
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = launchAff_ do
  writingToStdinWorks
  spawnLs
  nonExistentExecutable
  noEffectsTooEarly
  killsProcess
  execLs
  execSyncEcho "some value"

until
  :: forall emitter psCb jsCb a
   . emitter
  -> EventHandle emitter psCb jsCb
  -> ((a -> Effect Unit) -> psCb)
  -> Aff a
until ee event cb = makeAff \done -> do
  rm <- ee # once event (cb (done <<< Right))
  pure $ effectCanceler rm

writingToStdinWorks :: Aff Unit
writingToStdinWorks = do
  log "\nwriting to stdin works"
  sp <- liftEffect $ spawn "sh" [ "./test/sleep.sh" ]
  liftEffect do
    (stdin sp) # once_ Stream.errorH \err -> do
      log "Error in stdin"
      throwException $ unsafeCoerce err
    buf <- Buffer.fromString "helllo" UTF8
    void $ Stream.write (stdin sp) buf
    sp # once_ CP.errorH \err -> do
      log "Error in child process"
      throwException $ unsafeCoerce err
  exit <- until sp CP.closeH \completeAff -> \exit ->
    completeAff exit
  log $ "spawn sleep done " <> show exit

spawnLs :: Aff Unit
spawnLs = do
  log "\nspawns processes ok"
  ls <- liftEffect $ spawn "ls" [ "-la" ]
  res <- waitSpawned ls
  case res of
    Right pid -> log $ "ls successfully spawned with PID: " <> show pid
    Left err -> liftEffect $ throwException $ unsafeCoerce err
  exit <- until ls CP.closeH \complete -> \exit -> complete exit
  case exit of
    Normally 0 -> log $ "ls exited with 0"
    Normally i -> liftEffect $ throw $ "ls had non-zero exit: " <> show i
    BySignal sig -> liftEffect $ throw $ "ls exited with sig: " <> show sig

nonExistentExecutable :: Aff Unit
nonExistentExecutable = do
  log "\nemits an error if executable does not exist"
  ch <- liftEffect $ spawn "this-does-not-exist" []
  res <- waitSpawned ch
  case res of
    Left _ -> log "nonexistent executable: all good."
    Right pid -> liftEffect $ throw $ "nonexistent executable started with PID: " <> show pid

noEffectsTooEarly :: Aff Unit
noEffectsTooEarly = do
  log "\ndoesn't perform effects too early"
  ls <- liftEffect $ spawn "ls" [ "-la" ]
  let _ = kill ls
  exit <- until ls CP.exitH \complete -> \exit -> complete exit
  case exit of
    Normally 0 ->
      log "All good!"
    _ ->
      liftEffect $ throw $ "Bad exit: expected `Normally 0`, got: " <> show exit

killsProcess :: Aff Unit
killsProcess = do
  log "\nkills processes"
  ls <- liftEffect $ spawn "ls" [ "-la" ]
  _ <- liftEffect $ kill ls
  exit <- until ls CP.exitH \complete -> \exit -> complete exit
  case exit of
    BySignal s | Just SIGTERM <- Signal.fromString =<< (hush $ fromKillSignal s) ->
      log "All good!"
    _ -> do
      liftEffect $ throw $ "Bad exit: expected `BySignal SIGTERM`, got: " <> show exit

execLs :: Aff Unit
execLs = do
  log "\nexec"
  r <- makeAff \done -> do
    -- returned ChildProcess is ignored here
    void $ exec' "ls >&2" identity (done <<< Right)
    pure nonCanceler
  stdout' <- liftEffect $ Buffer.toString UTF8 r.stdout
  stderr' <- liftEffect $ Buffer.toString UTF8 r.stderr
  when (stdout' /= "") do
    liftEffect $ throw $ "stdout should be redirected to stderr but had content: " <> show stdout'
  when (stderr' == "") do
    liftEffect $ throw $ "stderr should have content but was empty"
  log "stdout was successfully redirected to stderr"

execSyncEcho :: String -> Aff Unit
execSyncEcho str = liftEffect do
  log "\nexecSyncEcho"
  buf <- Buffer.fromString str UTF8
  resBuf <- execSync' "cat" (_ { input = Just buf })
  res <- Buffer.toString NE.UTF8 resBuf
  when (str /= res) do
    throw $ "cat did not output its input. \nGot: " <> show res <> "\nExpected: " <> show str
  log "cat successfully re-outputted its input"
