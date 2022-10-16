{ name = "node-child-process"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "exceptions"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-streams"
  , "nullable"
  , "posix-types"
  , "prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
