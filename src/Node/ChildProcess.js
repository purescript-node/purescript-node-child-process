/* eslint-env node*/

import { spawn, exec, execFile, execSync, execFileSync, fork as cp_fork } from "child_process";

export function unsafeFromNullable(msg) {
  return x => {
    if (x === null) throw new Error(msg);
    return x;
  };
}

export const connectedImpl = (cp) => cp.connected;
export const disconnectImpl = (cp) => cp.disconnect();
export const exitCodeImpl = (cp) => cp.exitCode;
export const pidImpl = (cp) => cp.pid;
export const killImpl = (cp) => cp.kill();
export const killStrImpl = (cp, str) => cp.kill(str);
export const killedImpl = (cp) => cp.killed;
export const signalCodeImpl = (cp) => cp.signalCode;
export const spawnArgs = (cp) => cp.spawnArgs;
export const spawnFile = (cp) => cp.spawnFile;

export function spawnImpl(command) {
  return args => opts => () => spawn(command, args, opts);
}

export function execImpl(command) {
  return opts => callback => () => exec(
    command,
    opts,
    (err, stdout, stderr) => {
      callback(err)(stdout)(stderr)();
    }
  );
}

export const execFileImpl = function execImpl(command) {
  return args => opts => callback => () => execFile(
    command,
    args,
    opts,
    (err, stdout, stderr) => {
      callback(err)(stdout)(stderr)();
    }
  );
};

export function execSyncImpl(command) {
  return opts => () => execSync(command, opts);
}

export function execFileSyncImpl(command) {
  return args => opts => () => execFileSync(command, args, opts);
}

export function fork(cmd) {
  return args => () => cp_fork(cmd, args);
}

const _undefined = undefined;
export { _undefined as undefined };
import process from "process";
export { process };
