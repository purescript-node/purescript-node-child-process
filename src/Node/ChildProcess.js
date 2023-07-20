/* eslint-env node*/

import { spawn, exec, execFile, execSync, execFileSync, fork as cp_fork } from "child_process";

export function unsafeFromNullable(msg) {
  return x => {
    if (x === null) throw new Error(msg);
    return x;
  };
}

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
