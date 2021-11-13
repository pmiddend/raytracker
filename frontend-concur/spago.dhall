{ name = "my-project"
, dependencies =
  [ "concur-core"
  , "concur-react"
  , "console"
  , "effect"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
