let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall

in prelude.default ⫽
  { bin =
    [
      prelude.bin ⫽
      { src = "src/aeson-demo.dats"
      , target = "target/aeson-demo"
      , hsDeps = [ { projectFile = (["../cabal.project"] : Optional Text), cabalFile = "hs/aeson-demo.cabal", objectFile = "hs/AesonDemo.o" } ]
      , hs2ats = [ { hs = "hs/AesonDemo.hs", ats = ".atspkg/hs2ats/gen.sats", cpphs = False } ]
      }
    ]
    , dependencies = prelude.mapPlainDeps [ "hs-bind" ]
    , ccompiler = "ghc"
    , cflags = ["-optc-O2", "hs/AesonDemo"]
  }
