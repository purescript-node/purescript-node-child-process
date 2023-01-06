/* eslint-env node*/

export { 
  spawn as spawnImpl, 
  spawnSync as spawnSyncImpl,
  exec as execImpl, 
  execFile as execFileImpl, 
  execSync as execSyncImpl, 
  execFileSync as execFileSyncImpl, 
  fork as forkImpl 
} from "node:child_process";

export function channelImpl(cp) {
  return cp.channel;
}

export function connectedImpl(cp) {
  return cp.connected;
}

export function disconnectImpl(cp) {
  return cp.disconnect();
}

export function exitCodeImpl(cp) {
  return cp.exitCode;
}

export function killImpl(signal, cp) {
  return cp.kill(signal);
}

export function pidExistsImpl(cp) {
  return cp.kill(0);
}

export function killedImpl(cp) {
  return cp.killed;
}

export function pidImpl(cp) {
  return cp.pid;
}

export function refImpl(cp) {
  return cp.ref();
}

export function unrefImpl(cp) {
  return cp.unref();
}

export function sendImpl(cp, msg, handle, options, cb) {
  return cp.send(msg, handle, options, cb);
}

export function signalCodeImpl(cp) {
  return cp.signalCode;
}

export function spawnArgs(cp) {
  return cp.spawnArgs;
}

export function spawnFile(cp) {
  return cp.spawnFile;
}

export function stderrImpl(cp) {
  return cp.stderr;
}

export function stdinImpl(cp) {
  return cp.stdin;
}

export function stdioImpl(cp) {
  return cp.stdio;
}

export function stdoutImpl(cp) {
  return cp.stdout;
}

export function stdinImpl(cp) {
  return cp.stdin;
}

export function stdoutImpl(cp) {
  return cp.stdout;
}

export function stderrImpl(cp) {
  return cp.stderr;
}

export function onCloseImpl(cp, cb) {
  return cp.on("close", cb);
}

export function onDisconnectImpl(cp, cb) {
  return cp.on("disconnect", cb);
}

export function onErrorImpl(cp, cb) {
  return cp.on("error", cb);
}

export function onExitImpl(cp, cb) {
  return cp.on("exit", cb);
}

export function onMessageImpl(cp, cb) {
  return cp.on("message", cb);
}

export function onSpawnImpl(cp, cb) {
  return cp.on("spawn", cb);
}

const _undefined = undefined;
export { _undefined as undefined };
import process from "process";
export { process };
