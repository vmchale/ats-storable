%{^
#define STUB_H "hs/Foreign_stub.h"
#define STG_INIT __stginit_Foreign
%}

#include "$PATSHOMELOCS/hs-bind-0.4.2/runtime.dats"
#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

staload "libats/ML/SATS/string.sats"
staload UN = "prelude/SATS/unsafe.sats"
staload "$PATSHOMELOCS/gen/types.sats"

fun free_option(x : option(pair(int,int))) : void =
  case+ x of
    | ~Some (x) => ()
    | ~None() => ()

fun tostring_option_pair(x : !option(pair(int,int))) : string =
  case+ x of
    | None() => "None"
    | Some (x) => let
      var f = x.first
      var s = x.second
    in
      string_append5( "Some (Pair { first = "
                    , tostring_int(f)
                    , ", second = "
                    , tostring_int(s)
                    , " })"
                    )
    end

extern
fun hs_read(string) : ptr =
  "mac#read_dhall"

implement main0 (argc, argv) =
  {
    val _ = hs_init(argc, argv)
    var x = $UN.ptr0_get<option(pair(int,int))>(hs_read("./example.dhall"))
    val s = tostring_option_pair(x)
    val _ = println!(s)
    val _ = free_option(x)
    val _ = hs_exit()
  }
