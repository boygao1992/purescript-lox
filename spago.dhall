{ name = "purescript-lox"
, dependencies = [ "console", "effect", "parsing", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
