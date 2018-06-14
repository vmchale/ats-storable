#define ATS_MAINATSFLAG 1

datavtype option(a: t@ype+) =
  | Some of a
  | None

datavtype tri(a: t@ype+) =
  | First of a
  | Second
  | Third

typedef pair(a: t@ype, b: t@ype) = @{ first = a, second = b }
typedef product = pair(int, int)

extern
fun something() : option(product) =
  "mac#"

extern
fun something_else() : tri(int) =
  "mac#"

implement something () =
  let
    var x: product = @{ first = 1, second = 6 }
  in
    Some(x)
  end

implement something_else () =
  First(2)
