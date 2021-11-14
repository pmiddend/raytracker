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
  , "foldable-traversable"
  , "formatters"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "prelude"
  , "psci-support"
  , "read"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
