{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "argonaut-codecs"
  , "console"
  , "effect"
  , "fixed-precision"
  , "format"
  , "formatters"
  , "halogen"
  , "halogen-css"
  , "halogen-formless"
  , "halogen-portal"
  , "js-date"
  , "numbers"
  , "psci-support"
  , "rationals"
  , "routing"
  , "uuid"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
