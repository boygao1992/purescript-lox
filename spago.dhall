{ name = "purescript-lox"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "control"
  , "debugged"
  , "effect"
  , "either"
  , "identity"
  , "lists"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "maybe"
  , "strings"
  , "unicode"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
