"use strict";

// module Node.ChildProcess
/* eslint-env node*/

exports.spawnImpl = function spawnImpl(command) {
  return function(args) {
    return function(opts) {
      return function() {
        return require("child_process").spawn(command, args, opts);
      };
    };
  };
};
exports.mkOnExit = function mkOnExit(nothing){
  return function(just){
    return function(signalConstr){
      return function onExit(cp){
        return function(cb){
          return function(){
            cp.on("exit", function(code, signal){
              cb(code ? just(code) : nothing, signal ? just(signalConstr(signal)) : nothing);
            });
          };
        };
      };
    };
  };
};
exports.mkOnClose = function mkOnClose(nothing){
  return function(just){
    return function(signalConstr){
      return function onClose(cp){
        return function(cb){
          return function(){
            cp.on("close", function(code, signal){
              cb(code ? just(code) : nothing, signal ? just(signalConstr(signal)) : nothing);
            });
          };
        };
      };
    };
  };
};
exports.onDisconnect = function onDisconnect(cp){
  return function(cb){
    return function(){
      cp.on("disconnect", cb);
    };
  };
};
exports.mkOnMessage = function mkOnMessage(nothing){
  return function(just){
    return function onMessage(cp){
      return function(cb){
        return function(){
          cp.on("message", function(mess, sendHandle){
            cb(mess, sendHandle ? just(sendHandle) : nothing);
          });
        };
      };
    };
  };
};
exports.onError = function onError(cp){
  return function(cb){
    return function(){
      cp.on("error", cb);
    };
  };
};

exports["undefined"] = undefined;
exports.process = process;
