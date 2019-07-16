{ name =
    "hypertrout"
, dependencies =
    [ "argonaut-generic"
    , "console"
    , "hyper"
    , "prelude"
    , "psci-support"
    , "spec"
    , "spec-discovery"
    , "trout"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
