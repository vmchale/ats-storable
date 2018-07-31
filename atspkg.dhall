let prelude = http://hackage.haskell.org/package/ats-pkg/src/dhall/atspkg-prelude.dhall

in prelude.default ⫽
  { libraries =
    [
      prelude.lib ⫽
      { name = "storable"
      , src = [ "ats-src/types.dats" ]
      , libTarget = "dist-newstyle/lib/libstorable.a"
      , static = True
      }
    ]
  }
