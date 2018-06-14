let Option = < Some : { _1 : { first : Integer, second : Integer } } | None : {} >
in
let None = < Some : { _1 : { first : Integer, second : Integer } } | None = {=} >
in
let Some = λ(x : { first : Integer, second : Integer }) → < Some = { _1 = x } | None : {} >
in
let p = { first = +1, second = +6 }
in

Some p
