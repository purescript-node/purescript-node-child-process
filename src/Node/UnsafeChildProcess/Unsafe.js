export { 
  exec as execImpl,
  exec as execOptsImpl,
  exec as execCbImpl,
  exec as execOptsCbImpl,
  execFile as execFileImpl,
  execFile as execFileOptsImpl,
  execFile as execFileCbImpl,
  execFile as execFileOptsCbImpl,
  spawn as spawnImpl, 
  spawn as spawnOptsImpl, 
  execSync as execSyncImpl,
  execSync as execSyncOptsImpl,
  execFileSync as execFileSyncImpl,
  execFileSync as execFileSyncOptsImpl,
  spawnSync as spawnSyncImpl,
  spawnSync as spawnSyncOptsImpl,
  fork as forkImpl,
  fork as forkOptsImpl,
} from "node:child_process";

export const unsafeStdin = (cp) => cp.stdin;
export const unsafeStdout = (cp) => cp.stdout;
export const unsafeStderr = (cp) => cp.stderr;
export const unsafeChannelRefImpl = (cp) => cp.channel.ref();
export const unsafeChannelUnrefImpl = (cp) => cp.channel.unref();
export const sendImpl = (cp, msg, handle) => cp.send(msg, handle);
export const sendOptsImpl = (cp, msg, handle, opts) => cp.send(msg, handle, opts);
export const sendCbImpl = (cp, msg, handle, cb) => cp.send(msg, handle, cb);
export const sendOptsCbImpl = (cp, msg, handle, opts, cb) => cp.send(msg, handle, opts, cb);
