%{^
#define STUB_H "hs/AesonDemo_stub.h"
#define STG_INIT __stginit_AesonDemo
%}

#include "share/atspre_staload.hats"
#include "$PATSHOMELOCS/hs-bind-0.4.2/runtime.dats"

staload UN = "prelude/SATS/unsafe.sats"
staload ".atspkg/hs2ats/gen.sats"

fun free_yesp(x : young_english_speaking_person) : void =
  strptr_free(x.name)

fun print_p(j : !young_english_speaking_person) : void =
  {
    val _ = println!(j.name)
    val _ = println!(j.age)
  }

extern
fun hs_decode_json(string) : ptr =
  "mac#decode_json"

implement main0 (argc, argv) =
  {
    val _ = hs_init(argc, argv)
    val p = hs_decode_json("{ \"name\": \"Joe\", \"age\": 12 }")
    val _ = hs_exit()
    val j = $UN.ptr0_get<young_english_speaking_person>(p)
    val _ = print_p(j)
    val _ = free_yesp(j)
  }
