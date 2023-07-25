# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v11.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v11.0.0) - 2023-07-25

Breaking changes:
- Update the signal arg from `String` to `KillSignal` (#51, #52 by @JordanMartinez)

  - `Exit`'s `BySignal` constructor's arg
  - `exitH`/`closeH`'s signal arg
  - `spawnSync`'s `SpawnResult`'s `signal` field
  - `kill'`'s signal arg

New features:
- Added `fromKillSignal` (#51 by @JordanMartinez)

Other improvements:
- Fix regression: add `ref`/`unref` APIs that were dropped in `v10.0.0` (#50 by @JordanMartinez)

## [v10.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v10.0.0) - 2023-07-20

Breaking changes:
- Migrate `onEvent`-style event handlers to `eventH`-style (#43 by @JordanMartinez)

  ```purs
  -- Before
  onExit cp case _ of
    Normally exitCode -> ...
    BySignal signal -> ...

  -- After
  cp # on_ exitH case _ of
    Normally exitCode -> ...
    BySignal signal -> ...
  ```
  See https://pursuit.purescript.org/packages/purescript-node-event-emitter/3.0.0/docs/Node.EventEmitter for more details.
- Update `pid` type signature to return `Maybe Pid` rather than `Pid` (#44 by @JordanMartinez)
- Update `kill` returned value from `Effect Unit` to `Effect Boolean` (#44 by @JordanMartinez)
- Migrate `Error` to `node-os`' `SystemError` (#45 by @JordanMartinez)
- Breaking changes made to the `Exit` type (#46 by @JordanMartinez)

  - Moved from `Node.ChildProces` to `Node.ChildProces.Types`
  - Changed the `BySignal`'s constructor's arg type from `Signal` to `String`
- Breaking changes made to the `Handle` type (#46 by @JordanMartinez)
  
  - Moved from `Node.ChildProces` to `Node.ChildProces.Types`
- Converted `defaultOptions { override = Just 1}` pattern to `(_ { override = Just 1})`  (#46 by @JordanMartinez)

  Before:
  ```purs
  spawn "foo" [ "bar" ] (defaultSpawnOptions { someOption = Just overrideValue })
  spawn "foo" [ "bar" ] defaultSpawnOptions
  ```

  After:
  ```purs
  spawn "foo" [ "bar" ] (_ { someOption = Just overrideValue })
  spawn "foo" [ "bar" ] identity
  ```
- Restrict end-user's ability to configure `stdio` to only those appended to `safeStdio` (#46 by @JordanMartinez)

  See the module docs for `Node.ChildProcess`.
- All `ChildProcess`-creating functions have been updated to support no args and all args variants (#46, #48 by @JordanMartinez)

New features:
- Added event handler for `spawn` event (#43 by @JordanMartinez)
- Added missing APIs (#44 by @JordanMartinez)

  - exitCode
  - kill (no signal specified)
  - kill' (kill with a `String` signal)
  - killSignal (kill with an ADT `Signal` arg)
  - killed
  - signalCode
  - spawnArgs
  - spawnFile
- Added unsafe, uncurried API of all ChildProcess-creating functions (#46 by @JordanMartinez)
- Added safe variant of `spawnSync`/`spawnSync'` (#46 by @JordanMartinez)
- Added `Aff`-based `waitSpawned` to safely get `Pid` (#47 by @JordanMartinez)

  Blocks until child process either successfully spawns or fails to spawn.

Bugfixes:

Other improvements:
- Bumped CI's node version to `lts/*` (#41 by @JordanMartinez)
- Updated CI `actions/checkout` and `actions/setup-nodee` to `v3` (#41 by @JordanMartinez)
- Format codebase & enforce formatting in CI via purs-tidy (#42 by @JordanMartinez)
- Migrate FFI to uncurried functions (#44, #46 by @JordanMartinez)
- Updated recommended module alias in docs (#46 by @JordanMartinez)

## [v9.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v9.0.0) - 2022-04-29

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#31 by @JordanMartinez, @thomashoneyman, @sigma-andex)

## [v8.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v8.0.0) - 2022-04-27

Due to an incorrectly-made breaking change, please use v8.0.0 instead.

## [v7.1.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v7.1.0) - 2021-07-13

New features:

- Added `shell` and `encoding` options to `exec` functions (#29 by @thomashoneyman)

## [v7.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v7.0.0) - 2021-02-26

Breaking changes:

- Updated dependencies for PureScript 0.14 (#25)

Other improvements:

- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#24)
- Added a CHANGELOG.md file and pull request template (#26)

## [v6.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v6.0.0) - 2019-03-15

- Updated `purescript-foreign-object` dependency

## [v5.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v5.0.0) - 2018-06-02

- Updates for 0.12

**Breaking**

- mkOnClose now reacts to the `close` signal instead of `exit` @Profpatsch
- `kill` returns Unit instead of Boolean @Profpatsch
- `exec`/`execFile` returns a ChildProcess @Profpatsch

**Additions**

- Bindings to the synchronous versions of exec @jyh1

## [v4.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v4.0.0) - 2017-04-05

- Updates for 0.11 compiler

## [v3.0.1](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v3.0.1) - 2016-11-19

- Fixed shadowed name warning

## [v3.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v3.0.0) - 2016-10-22

- Updated dependencies

## [v2.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v2.0.0) - 2016-07-31

- Updated dependencies

## [v1.0.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v1.0.0) - 2016-06-19

- Updates for 0.9.1 compiler and 1.0 core libraries.

## [v0.6.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.6.0) - 2016-03-31

- Bump dependencies (`purescript-node-streams` -> v0.4.0).

## [v0.5.1](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.5.1) - 2016-01-14

- Add `execFile`

## [v0.5.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.5.0) - 2016-01-06

- Bump dependencies (`node-fs` -> `~0.10.0`)

## [v0.4.2](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.4.2) - 2016-01-01

- Fix `Node_ChildProcess.fork is not a function` errors when calling `Node.ChildProcess.fork`
- Fix unused import warnings

## [v0.4.1](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.4.1) - 2015-12-31

- Fix `onError` never firing. Oops.

## [v0.4.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.4.0) - 2015-12-29

- **Breaking change**:

  - `SpawnOptions` now uses the `Uid` and `Gid` types from `purescript-posix-types` for its `uid` and `gid` options, instead of `Int`.

- **New features**:
  - Added `exec`.

## [v0.3.2](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.3.2) - 2015-12-27

- Documentation updates

## [v0.3.1](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.3.1) - 2015-12-27

- Move documentation to Pursuit
- Documentation updates

## [v0.3.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.3.0) - 2015-12-27

- Use purescript-posix-types
- Fix a bug where `kill`, `connected`, and `send` would perform the effects too early
- Modify type names to avoid repeating the module name: `ChildProcessExit` -> `Exit`, and `ChildProcessError` -> `Error`.

## [v0.2.0](https://github.com/purescript-node/purescript-node-child-process/releases/tag/v0.2.0) - 2015-12-27

- Use a `StrMap String` for child process environments in `spawn`, in order to ensure environment variable values are strings
- Allow inheritance of parent process environment by passing `Nothing` to the `env` parameter
- Use an opaque data type for `ChildProcess` values
- Use `Int` instead of `Number` where applicable (eg, `gid`, `uid`, `pid`)
- Require `Eff` for reading mutable state of a `ChildProcess`
- Simplify effects; now, we just have `cp :: CHILD_PROCESS` for spawning and communicating with child processes
- Use a sum type to allow more flexibility with what to do with standard IO streams / file descriptors in the child process after spawning
- Fix a bug where callbacks in `onExit` and `onClose` did not get called
- Add a `ChildProcessExit` type with information about how a child process exited
- Fix warnings
- Update dependencies: `purescript-node-streams` -> `~0.3.0`

See https://github.com/joneshf/purescript-node-child-process/issues/2 for the rationale behind many of these changes.
