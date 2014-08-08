# Module Documentation

## Module Node.ChildProcess

### Types

    newtype ChildProcess where
      ChildProcess :: ChildProcessRec -> ChildProcess

    type ChildProcessRec  = { disconnect :: forall eff. Fn0 (Eff eff Unit), send :: forall eff r. Fn2 {  | r } Handle (Eff eff Unit), kill :: forall eff. Fn1 Signal Boolean, connected :: Boolean, pid :: Number, stdout :: Stream Stdout, stdin :: Stream Stdin, stderr :: Stream Stderr }

    data Handle :: *

    data Spawn :: !

    type SpawnOptions  = { gid :: Number, uid :: Number, detached :: Boolean, env :: forall r. {  | r }, stdio :: [String], cwd :: String }

    data Stderr :: !

    data Stdin :: !

    data Stdout :: !

    data Stream :: ! -> *


### Type Class Instances

    instance eventEmitterChildProcess :: EventEmitter ChildProcess

    instance eventEmitterStreamStderr :: EventEmitter (Stream Stderr)

    instance eventEmitterStreamStdin :: EventEmitter (Stream Stdin)

    instance eventEmitterStreamStdout :: EventEmitter (Stream Stdout)


### Values

    closeEvent :: Event

    defaultSpawnOptions :: SpawnOptions

    disconnectEvent :: Event

    errorEvent :: Event

    exitEvent :: Event

    messageEvent :: Event

    process :: forall r. { env :: {  | r } }

    spawn :: forall eff. String -> [String] -> SpawnOptions -> Eff (spawn :: Spawn | eff) ChildProcess

    undefined :: forall a. a


## Module Node.ChildProcess.Signal

### Types

    newtype Signal


### Type Class Instances

    instance showSignal :: Show Signal



