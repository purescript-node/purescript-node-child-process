"use strict";

/* eslint-env node*/
import childProcess from "child_process";

export function unsafeFromNullable(msg) {
  return function (x) {
    if (x === null) throw new Error(msg);
    return x;
  };
}

export function spawnImpl(command) {
  return function (args) {
    return function (opts) {
      return function () {
        return childProcess.spawn(command, args, opts);
      };
    };
  };
}

export function execImpl(command) {
  return function (opts) {
    return function (callback) {
      return function () {
        return childProcess.exec(
          command,
          opts,
          function (err, stdout, stderr) {
            callback(err)(stdout)(stderr)();
          }
        );
      };
    };
  };
}

export function execImpl(command) {
  return function (args) {
    return function (opts) {
      return function (callback) {
        return function () {
          return childProcess.execFile(
            command,
            args,
            opts,
            function (err, stdout, stderr) {
              callback(err)(stdout)(stderr)();
            }
          );
        };
      };
    };
  };
};

export function execSyncImpl(command) {
  return function (opts) {
    return function () {
      return childProcess.execSync(command, opts);
    };
  };
}

export function execFileSyncImpl(command) {
  return function (args) {
    return function (opts) {
      return function () {
        return childProcess.execFileSync(command, args, opts);
      };
    };
  };
}

export function fork(cmd) {
  return function (args) {
    return function () {
      return childProcess.fork(cmd, args);
    };
  };
}

export function mkOnExit(mkChildExit) {
  return function onExit(cp) {
    return function (cb) {
      return function () {
        cp.on("exit", function (code, signal) {
          cb(mkChildExit(code)(signal))();
        });
      };
    };
  };
}

export function mkOnClose(mkChildExit) {
  return function onClose(cp) {
    return function (cb) {
      return function () {
        cp.on("close", function (code, signal) {
          cb(mkChildExit(code)(signal))();
        });
      };
    };
  };
}

export function onDisconnect(cp) {
  return function (cb) {
    return function () {
      cp.on("disconnect", cb);
    };
  };
}

export function mkOnMessage(nothing) {
  return function (just) {
    return function onMessage(cp) {
      return function (cb) {
        return function () {
          cp.on("message", function (mess, sendHandle) {
            cb(mess, sendHandle ? just(sendHandle) : nothing)();
          });
        };
      };
    };
  };
}

export function onError(cp) {
  return function (cb) {
    return function () {
      cp.on("error", function (err) {
        cb(err)();
      });
    };
  };
}

export { undefined };
export { process };
