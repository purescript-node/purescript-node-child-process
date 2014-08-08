module Node.ChildProcess.Signal
  ( Signal()
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

  newtype Signal = Signal String

  sigabrt   = Signal "SIGABRT"
  sigalrm   = Signal "SIGALRM"
  sigbus    = Signal "SIGBUS"
  sigchld   = Signal "SIGCHLD"
  sigcld    = Signal "SIGCLD"
  sigcont   = Signal "SIGCONT"
  sigemt    = Signal "SIGEMT"
  sigfpe    = Signal "SIGFPE"
  sighup    = Signal "SIGHUP"
  sigill    = Signal "SIGILL"
  siginfo   = Signal "SIGINFO"
  sigint    = Signal "SIGINT"
  sigio     = Signal "SIGIO"
  sigiot    = Signal "SIGIOT"
  sigkill   = Signal "SIGKILL"
  siglost   = Signal "SIGLOST"
  sigpipe   = Signal "SIGPIPE"
  sigpoll   = Signal "SIGPOLL"
  sigprof   = Signal "SIGPROF"
  sigpwr    = Signal "SIGPWR"
  sigquit   = Signal "SIGQUIT"
  sigsegv   = Signal "SIGSEGV"
  sigstkflt = Signal "SIGSTKFLT"
  sigstop   = Signal "SIGSTOP"
  sigsys    = Signal "SIGSYS"
  sigterm   = Signal "SIGTERM"
  sigtrap   = Signal "SIGTRAP"
  sigtstp   = Signal "SIGTSTP"
  sigttin   = Signal "SIGTTIN"
  sigttou   = Signal "SIGTTOU"
  sigunused = Signal "SIGUNUSED"
  sigurg    = Signal "SIGURG"
  sigusr1   = Signal "SIGUSR1"
  sigusr2   = Signal "SIGUSR2"
  sigvtalrm = Signal "SIGVTALRM"
  sigwinch  = Signal "SIGWINCH"
  sigxcpu   = Signal "SIGXCPU"
  sigxfsz   = Signal "SIGXFSZ"

  instance showSignal :: Show Signal where
    show (Signal sig) = sig
