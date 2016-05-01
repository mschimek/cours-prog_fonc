type 'value m


val bind : 'value m -> ('value -> 'result m) -> 'result m

val return : 'value -> 'value m


module Infix : sig
  val (>>=) : 'value m -> ('value -> 'result m) -> 'result m
  val (>>) : unit m -> 'result m -> 'result m
end

val run_with_continuation : 'value m -> ('value -> 'result) -> 'result

val run : 'value m -> 'value
