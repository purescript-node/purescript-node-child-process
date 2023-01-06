const newImpl = function() {
  return new AbortController();
};

export { newImpl as new };

export function abortImpl(ac) {
  return ac.abort();
}

export function abortReasonImpl(ac, reason) {
  return ac.abort(reason);
}

export function signal(ac) {
  return ac.signal;
}

export function newSignalImpl(reason) {
  return AbortSignal.abort(reason);
}

export function newDelayImpl(timeout) {
  return AbortSignal.timeout(timeout);
}

export function onAbortImpl(signal, cb) {
  return signal.onabort(cb);
}

export function abortedImpl(signal) {
  return signal.aborted;
}

export function reasonImpl(signal) {
  return signal.reason;
}

export function throwIfAbortedImpl(signal) {
  try {
    signal.throwIfAbortedImpl();
    return null;
  } catch (reason) {
    return reason;
  }
}

export function unsafeThrowIfAbortedImpl(signal) {
  return signal.throwIfAbortedImpl();
}
