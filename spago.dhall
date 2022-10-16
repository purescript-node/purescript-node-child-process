{ name = "node-child-process"
, dependencies =
  [ "aff"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-streams"
  , "node-streams-aff"
  , "nullable"
  , "parallel"
  , "posix-types"
  , "prelude"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
