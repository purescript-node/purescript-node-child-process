module Node.ChildProcess.Signal
  ( Signal(..)
  , sigabrt
  , sigalrm
  , sigbus
  , sigchld
  , sigcld
  , sigcont
  , sigemt
  , sigfpe
  , sighup
  , sigill
  , siginfo
  , sigint
  , sigio
  , sigiot
  , sigkill
  , siglost
  , sigpipe
  , sigpoll
  , sigprof
  , sigpwr
  , sigquit
  , sigsegv
  , sigstkflt
  , sigstop
  , sigsys
  , sigterm
  , sigtrap
  , sigtstp
  , sigttin
  , sigttou
  , sigunused
  , sigurg
  , sigusr1
  , sigusr2
  , sigvtalrm
  , sigwinch
  , sigxcpu
  , sigxfsz
  ) where

import Prelude (Show)

newtype Signal = Signal String

sigabrt  :: Signal
sigabrt   = Signal "SIGABRT"
sigalrm  :: Signal
sigalrm   = Signal "SIGALRM"
sigbus   :: Signal
sigbus    = Signal "SIGBUS"
sigchld  :: Signal
sigchld   = Signal "SIGCHLD"
sigcld   :: Signal
sigcld    = Signal "SIGCLD"
sigcont  :: Signal
sigcont   = Signal "SIGCONT"
sigemt   :: Signal
sigemt    = Signal "SIGEMT"
sigfpe   :: Signal
sigfpe    = Signal "SIGFPE"
sighup   :: Signal
sighup    = Signal "SIGHUP"
sigill   :: Signal
sigill    = Signal "SIGILL"
siginfo  :: Signal
siginfo   = Signal "SIGINFO"
sigint   :: Signal
sigint    = Signal "SIGINT"
sigio    :: Signal
sigio     = Signal "SIGIO"
sigiot   :: Signal
sigiot    = Signal "SIGIOT"
sigkill  :: Signal
sigkill   = Signal "SIGKILL"
siglost  :: Signal
siglost   = Signal "SIGLOST"
sigpipe  :: Signal
sigpipe   = Signal "SIGPIPE"
sigpoll  :: Signal
sigpoll   = Signal "SIGPOLL"
sigprof  :: Signal
sigprof   = Signal "SIGPROF"
sigpwr   :: Signal
sigpwr    = Signal "SIGPWR"
sigquit  :: Signal
sigquit   = Signal "SIGQUIT"
sigsegv  :: Signal
sigsegv   = Signal "SIGSEGV"
sigstkflt:: Signal
sigstkflt = Signal "SIGSTKFLT"
sigstop  :: Signal
sigstop   = Signal "SIGSTOP"
sigsys   :: Signal
sigsys    = Signal "SIGSYS"
sigterm  :: Signal
sigterm   = Signal "SIGTERM"
sigtrap  :: Signal
sigtrap   = Signal "SIGTRAP"
sigtstp  :: Signal
sigtstp   = Signal "SIGTSTP"
sigttin  :: Signal
sigttin   = Signal "SIGTTIN"
sigttou  :: Signal
sigttou   = Signal "SIGTTOU"
sigunused:: Signal
sigunused = Signal "SIGUNUSED"
sigurg   :: Signal
sigurg    = Signal "SIGURG"
sigusr1  :: Signal
sigusr1   = Signal "SIGUSR1"
sigusr2  :: Signal
sigusr2   = Signal "SIGUSR2"
sigvtalrm:: Signal
sigvtalrm = Signal "SIGVTALRM"
sigwinch :: Signal
sigwinch  = Signal "SIGWINCH"
sigxcpu  :: Signal
sigxcpu   = Signal "SIGXCPU"
sigxfsz  :: Signal
sigxfsz   = Signal "SIGXFSZ"

instance showSignal :: Show Signal where
  show (Signal sig) = sig
