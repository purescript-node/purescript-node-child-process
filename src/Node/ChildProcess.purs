module Node.ChildProcess
  ( Handle()
  , Spawn()
  , Stdin()
  , Stdout()
  , Stderr()
  , ChildProcess()
  , SpawnOptions()
  , ChildProcessError()
  , onExit
  , onClose
  , onDisconnect
  , onMessage
  , onError
  , spawn
  , defaultSpawnOptions
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Function (Fn0(), Fn1(), Fn2())
import Data.Maybe (Maybe(..))
import Data.Foreign (Foreign())

import Node.Stream (Readable(), Writable())
import Node.ChildProcess.Signal (Signal(..))

foreign import data Handle :: *
foreign import data Spawn :: !
foreign import data Stdin :: !
foreign import data Stdout :: !
foreign import data Stderr :: !

type ChildProcess =
  { stderr     :: forall eff. Readable () (stderr :: Stderr | eff) String
  , stdin      :: forall eff. Writable () (stdin :: Stdin | eff) String
  , stdout     :: forall eff. Readable  () (stdout :: Stdout | eff) String
  , pid        :: Number
  , connected  :: Boolean
  , kill       :: forall eff. Fn1 Signal Boolean
  , send       :: forall eff r. Fn2 { | r} Handle (Eff eff Unit)
  , disconnect :: forall eff. Fn0 (Eff eff Unit)
  }

type SpawnOptions =
  { cwd       :: String
  , stdio     :: Array String
  , env       :: forall r. { | r}
  , detached  :: Boolean
  , uid       :: Number
  , gid       :: Number
  }

onExit :: forall eff. ChildProcess -> (Maybe Number -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
onExit = mkOnExit Nothing Just Signal

onClose :: forall eff. ChildProcess -> (Maybe Number -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
onClose = mkOnClose Nothing Just Signal

onMessage :: forall eff.  ChildProcess -> (Foreign -> Maybe Handle -> Eff eff Unit) -> Eff eff Unit
onMessage = mkOnMessage Nothing Just

foreign import onDisconnect :: forall eff. ChildProcess -> Eff eff Unit -> Eff eff Unit
foreign import onError :: forall eff. ChildProcess -> (ChildProcessError -> Eff eff Unit) -> Eff eff Unit

foreign import spawn :: forall eff. String -> Array String -> SpawnOptions -> Eff (spawn :: Spawn | eff) ChildProcess

defaultSpawnOptions :: SpawnOptions
defaultSpawnOptions =
  { cwd: undefined
  , stdio: ["pipe", "pipe", "pipe"]
  , env: process.env
  , detached: false
  , uid: undefined
  , gid: undefined
  }

type ChildProcessError = 
  { code :: String
  , errno :: String
  , syscall :: String
  }

foreign import mkOnExit :: forall a eff.
          Maybe a -> (a -> Maybe a) -> (String -> Signal) ->
          ChildProcess -> (Maybe Number -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit
foreign import mkOnMessage :: forall a eff. 
          Maybe a -> (a -> Maybe a) -> 
          ChildProcess -> (Foreign -> Maybe Handle -> Eff eff Unit) -> Eff eff Unit
foreign import mkOnClose :: forall a eff.
          Maybe a -> (a -> Maybe a) -> (String -> Signal) ->
          ChildProcess -> (Maybe Number -> Maybe Signal -> Eff eff Unit) -> Eff eff Unit

-- There's gotta be a better way.
foreign import undefined :: forall a. a
foreign import process :: forall r. {env :: { | r}}
