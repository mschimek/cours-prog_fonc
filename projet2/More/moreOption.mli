val bind : 'elt option -> ('elt -> 'result option) -> 'result option

val default: 'elt -> 'elt option -> 'elt

val map : ('elt -> 'result) -> 'elt option -> 'result option

val guard : ('elt -> bool) -> 'elt option -> 'elt option

val do_if_defined : 'elt option -> ('elt -> unit) -> unit

val of_option_list : 'elt option list -> 'elt list option

val flatten : 'elt option option -> 'elt option

val product : 'left option -> 'right option -> ('left * 'right) option

module Infix : sig
  val (>>=) : 'elt option -> ('elt -> 'result option) -> 'result option
end
