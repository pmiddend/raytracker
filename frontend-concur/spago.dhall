{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "concur-core"
  , "concur-react"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "formatters"
  , "lists"
  , "maybe"
  , "now"
  , "prelude"
  , "psci-support"
  , "read"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
