module Node.AbortController where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)

foreign import data AbortController :: Type

instance Show AbortController where
  show _ = "AbortController"

foreign import new :: Effect (AbortController)

abort :: AbortController -> Effect Unit
abort ac = runEffectFn1 abortImpl ac

foreign import abortImpl :: EffectFn1 (AbortController) (Unit)

abort' :: AbortController -> Foreign -> Effect Unit
abort' ac reason' = runEffectFn2 abortReasonImpl ac reason'

foreign import abortReasonImpl :: EffectFn2 (AbortController) (Foreign) (Unit)

foreign import data AbortSignal :: Type

instance Show AbortSignal where
  show _ = "AbortSignal"

foreign import signal :: AbortController -> AbortSignal

newSignal :: Foreign -> Effect AbortSignal
newSignal reason' = runEffectFn1 newSignalImpl reason'

foreign import newSignalImpl :: EffectFn1 (Foreign) (AbortSignal)

newDelay :: Int -> Effect AbortSignal
newDelay delay = runEffectFn1 newDelayImpl delay

foreign import newDelayImpl :: EffectFn1 (Int) (AbortSignal)

onAbort :: AbortSignal -> Effect Unit -> Effect Unit
onAbort signal' cb = runEffectFn2 onAbortImpl signal' cb

foreign import onAbortImpl :: EffectFn2 (AbortSignal) (Effect Unit) (Unit)

aborted :: AbortSignal -> Effect Boolean
aborted signal' = runEffectFn1 abortedImpl signal'

foreign import abortedImpl :: EffectFn1 (AbortSignal) (Boolean)

reason :: AbortSignal -> Effect (Maybe Foreign)
reason signal' = toMaybe <$> runEffectFn1 reasonImpl signal'

foreign import reasonImpl :: EffectFn1 (AbortSignal) (Nullable Foreign)

throwIfAborted :: AbortSignal -> Effect (Maybe Foreign)
throwIfAborted signal' = toMaybe <$> runEffectFn1 throwIfAbortedImpl signal'

foreign import throwIfAbortedImpl :: EffectFn1 (AbortSignal) (Nullable Foreign)

unsafeThrowIfAborted :: AbortSignal -> Effect Unit
unsafeThrowIfAborted signal' = runEffectFn1 unsafeThrowIfAbortedImpl signal'

foreign import unsafeThrowIfAbortedImpl :: EffectFn1 (AbortSignal) Unit
