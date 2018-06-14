let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall

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
