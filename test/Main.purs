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
import Effect.Exception (throwException)
import Node.Buffer as Buffer
import Node.ChildProcess (exec', execSync', kill, spawn, stdin, stdout)
import Node.ChildProcess as CP
import Node.ChildProcess.Types (Exit(..), fromKillSignal)
import Node.Encoding (Encoding(..))
import Node.Encoding as NE
import Node.Errors.SystemError (code)
import Node.EventEmitter (EventHandle, on_, once, once_)
import Node.Stream (dataH)
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
  liftEffect $ (stdout ls) # on_ dataH (Buffer.toString UTF8 >=> log)
  exit <- until ls CP.exitH \complete -> \exit -> complete exit
  log $ "ls exited: " <> show exit

nonExistentExecutable :: Aff Unit
nonExistentExecutable = do
  log "\nemits an error if executable does not exist"
  ch <- liftEffect $ spawn "this-does-not-exist" []
  err <- until ch CP.errorH \complete -> \err -> complete err
  log (code err)
  log "nonexistent executable: all good."

noEffectsTooEarly :: Aff Unit
noEffectsTooEarly = do
  log "\ndoesn't perform effects too early"
  ls <- liftEffect $ spawn "ls" [ "-la" ]
  let _ = kill ls
  exit <- until ls CP.exitH \complete -> \exit -> complete exit
  case exit of
    Normally 0 ->
      log "All good!"
    _ -> do
      log ("Bad exit: expected `Normally 0`, got: " <> show exit)

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
      log ("Bad exit: expected `BySignal SIGTERM`, got: " <> show exit)

execLs :: Aff Unit
execLs = do
  log "\nexec"
  r <- makeAff \done -> do
    -- returned ChildProcess is ignored here
    void $ exec' "ls >&2" identity (done <<< Right)
    pure nonCanceler
  log "redirected to stderr:" *> (liftEffect $ Buffer.toString UTF8 r.stderr >>= log)

execSyncEcho :: String -> Aff Unit
execSyncEcho str = liftEffect do
  log "\nexecSyncEcho"
  buf <- Buffer.fromString str UTF8
  resBuf <- execSync' "cat" (_ { input = Just buf })
  res <- Buffer.toString NE.UTF8 resBuf
  log res
