module Node.ChildProcess where

  import Control.Events (Event(..), EventEmitter)
  import Control.Monad.Eff (Eff())

  import Data.Function (Fn0(), Fn1(), Fn2())

  import Node.ChildProcess.Signal (Signal(..))

  foreign import data Handle :: *
  foreign import data Spawn :: !
  foreign import data Stream :: ! -> *
  foreign import data Stderr :: !
  foreign import data Stdin :: !
  foreign import data Stdout :: !

  newtype ChildProcess = ChildProcess ChildProcessRec

  type ChildProcessRec =
    { stderr     :: Stream Stderr
    , stdin      :: Stream Stdin
    , stdout     :: Stream Stdout
    , pid        :: Number
    , connected  :: Boolean
    , kill       :: forall eff. Fn1 Signal Boolean
    , send       :: forall eff r. Fn2 { | r} Handle (Eff eff Unit)
    , disconnect :: forall eff. Fn0 (Eff eff Unit)
    }

  type SpawnOptions =
    { cwd       :: String
    , stdio     :: [String]
    , env       :: forall r. { | r}
    , detached  :: Boolean
    , uid       :: Number
    , gid       :: Number
    }

  instance eventEmitterStreamStderr :: EventEmitter (Stream Stderr)
  instance eventEmitterStreamStdin  :: EventEmitter (Stream Stdin)
  instance eventEmitterStreamStdout :: EventEmitter (Stream Stdout)

  instance eventEmitterChildProcess :: EventEmitter ChildProcess

  closeEvent :: Event
  closeEvent = Event "close"

  disconnectEvent :: Event
  disconnectEvent = Event "disconnect"

  errorEvent :: Event
  errorEvent = Event "error"

  exitEvent :: Event
  exitEvent = Event "exit"

  messageEvent :: Event
  messageEvent = Event "message"

  foreign import spawn
    "function spawn(command) {\
    \  return function(args) {\
    \    return function(opts) {\
    \      return function() {\
    \        return require('child_process').spawn(command, args, opts);\
    \      }\
    \    }\
    \  }\
    \}" :: forall eff. String -> [String] -> SpawnOptions -> Eff (spawn :: Spawn | eff) ChildProcess

  defaultSpawnOptions :: SpawnOptions
  defaultSpawnOptions =
    { cwd: undefined
    , stdio: ["pipe", "pipe", "pipe"]
    , env: process.env
    , detached: false
    , uid: undefined
    , gid: undefined
    }

  -- There's gotta be a better way.
  foreign import undefined :: forall a. a
  foreign import process :: forall r. {env :: { | r}}
