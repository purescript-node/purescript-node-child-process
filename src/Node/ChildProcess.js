"use strict";

/* eslint-env node*/

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
        return require("child_process").spawn(command, args, opts);
      };
    };
  };
}

export function execImpl(command) {
  return function (opts) {
    return function (callback) {
      return function () {
        return require("child_process").exec(
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

exports.execFileImpl = function execImpl(command) {
  return function (args) {
    return function (opts) {
      return function (callback) {
        return function () {
          return require("child_process").execFile(
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
      return require("child_process").execSync(command, opts);
    };
  };
}

export function execFileSyncImpl(command) {
  return function (args) {
    return function (opts) {
      return function () {
        return require("child_process").execFileSync(command, args, opts);
      };
    };
  };
}

export function fork(cmd) {
  return function (args) {
    return function () {
      return require("child_process").fork(cmd, args);
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

export {undefined};
export {process};
