{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "parsing-foreign"
, dependencies =
  [ "console"
  , "effect"
  , "ffi-foreign"
  , "foreign"
  , "parsing"
  , "parsing-validation"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
