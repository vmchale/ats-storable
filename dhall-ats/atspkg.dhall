let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall

in prelude.default ⫽
  { bin =
    [
      prelude.bin ⫽
      { src = "src/dhall-ats.dats"
      , target = "target/dhall-ats"
      , hsDeps = [ { cabalFile = "hs/foreign.cabal", objectFile = "hs/Foreign.o", projectFile = ([ "../cabal.project" ] : Optional Text) } ]
      , hs2ats = [ { hs = "hs/Foreign.hs", ats = ".atspkg/contrib/gen/types.sats", cpphs = False } ]
      }
    ]
    , dependencies = prelude.mapPlainDeps [ "hs-bind" ]
    , ccompiler = "ghc-8.4.3"
    , cflags = [ "-optc-O2", "hs/Foreign" ]
  }
