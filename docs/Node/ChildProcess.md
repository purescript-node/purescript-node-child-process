## Module Node.ChildProcess

#### `Handle`

``` purescript
data Handle :: *
```

#### `Spawn`

``` purescript
data Spawn :: !
```

#### `Stdin`

``` purescript
data Stdin :: !
```

#### `Stdout`

``` purescript
data Stdout :: !
```

#### `Stderr`

``` purescript
data Stderr :: !
```

#### `ChildProcess`

``` purescript
type ChildProcess = { stderr :: forall eff. Readable () (stderr :: Stderr | eff) String, stdin :: forall eff. Writable () (stdin :: Stdin | eff) String, stdout :: forall eff. Readable () (stdout :: Stdout | eff) String, pid :: Number, connected :: Boolean, kill :: forall eff. Fn1 Signal Boolean, send :: forall eff r. Fn2 {  | r } Handle (Eff eff Unit), disconnect :: forall eff. Fn0 (Eff eff Unit) }
```

#### `SpawnOptions`

``` purescript
type SpawnOptions = { cwd :: String, stdio :: Array String, env :: forall r. {  | r }, detached :: Boolean, uid :: Number, gid :: Number }
```

#### `onExit`

``` purescript
onExit :: forall eff. ChildProcess -> (Maybe Number -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
```

#### `onClose`

``` purescript
onClose :: forall eff. ChildProcess -> (Maybe Number -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
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
spawn :: forall eff. String -> Array String -> SpawnOptions -> Eff (spawn :: Spawn | eff) ChildProcess
```

#### `defaultSpawnOptions`

``` purescript
defaultSpawnOptions :: SpawnOptions
```

#### `ChildProcessError`

``` purescript
type ChildProcessError = { code :: String, errno :: String, syscall :: String }
```


