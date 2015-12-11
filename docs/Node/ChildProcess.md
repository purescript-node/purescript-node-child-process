## Module Node.ChildProcess

#### `Handle`

``` purescript
data Handle :: *
```

A handle for inter-process communication (IPC).

#### `CHILD_PROCESS`

``` purescript
data CHILD_PROCESS :: !
```

The effect for creating and interacting with child processes.

#### `ChildProcess`

``` purescript
newtype ChildProcess
```

#### `stderr`

``` purescript
stderr :: forall eff. ChildProcess -> Readable () (cp :: CHILD_PROCESS | eff) Buffer
```

The standard error stream of a child process. Note that this is only
available if the process was spawned with the stderr option set to "pipe".

#### `stdout`

``` purescript
stdout :: forall eff. ChildProcess -> Readable () (cp :: CHILD_PROCESS | eff) Buffer
```

The standard output stream of a child process. Note that this is only
available if the process was spawned with the stdout option set to "pipe".

#### `stdin`

``` purescript
stdin :: forall eff. ChildProcess -> Writable () (cp :: CHILD_PROCESS | eff) Buffer
```

The standard input stream of a child process. Note that this is only
available if the process was spawned with the stdin option set to "pipe".

#### `pid`

``` purescript
pid :: ChildProcess -> Int
```

The process ID of a child process. Note that if the process has already
exited, another process may have taken the same ID, so be careful!

#### `connected`

``` purescript
connected :: forall eff. ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
```

#### `send`

``` purescript
send :: forall eff props. {  | props } -> Handle -> ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
```

#### `disconnect`

``` purescript
disconnect :: forall eff. ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Unit
```

#### `kill`

``` purescript
kill :: forall eff. Signal -> ChildProcess -> Eff (cp :: CHILD_PROCESS | eff) Boolean
```

Send a signal to a child process. It's an unfortunate historical decision
that this function is called "kill", as sending a signal to a child
process won't necessarily kill it.

#### `SpawnOptions`

``` purescript
type SpawnOptions = { cwd :: Maybe String, stdio :: Array (Maybe StdIOBehaviour), env :: Maybe (StrMap String), detached :: Boolean, uid :: Maybe Int, gid :: Maybe Int }
```

#### `onExit`

``` purescript
onExit :: forall eff. ChildProcess -> (Maybe Int -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
```

#### `onClose`

``` purescript
onClose :: forall eff. ChildProcess -> (Maybe Int -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
```

#### `onMessage`

``` purescript
onMessage :: forall eff. ChildProcess -> (Foreign -> Maybe Handle -> Eff eff Unit) -> Eff eff Unit
```

#### `onDisconnect`

``` purescript
onDisconnect :: forall eff. ChildProcess -> Eff eff Unit -> Eff eff Unit
```

#### `onError`

``` purescript
onError :: forall eff. ChildProcess -> (ChildProcessError -> Eff eff Unit) -> Eff eff Unit
```

#### `spawn`

``` purescript
spawn :: forall eff. String -> Array String -> SpawnOptions -> Eff (cp :: CHILD_PROCESS | eff) ChildProcess
```

#### `defaultSpawnOptions`

``` purescript
defaultSpawnOptions :: SpawnOptions
```

#### `ChildProcessError`

``` purescript
type ChildProcessError = { code :: String, errno :: String, syscall :: String }
```

An error which occurred inside a child process.

#### `StdIOBehaviour`

``` purescript
data StdIOBehaviour
```

Behaviour for standard IO streams (eg, standard input, standard output) of
a child process.

* `Pipe`: creates a pipe between the child and parent process, which can
  then be accessed as a `Stream` via the `stdin`, `stdout`, or `stderr`
  functions.
* `Ignore`: ignore this stream. This will cause Node to open /dev/null and
  connect it to the stream.
* `ShareStream`: Connect the supplied stream to the corresponding file
   descriptor in the child.
* `ShareFD`: Connect the supplied file descriptor (which should be open
  in the parent) to the corresponding file descriptor in the child.


