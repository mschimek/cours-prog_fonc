val find : predicate:('elt -> bool) -> 'elt list -> 'elt option

val find_minimum : objective:('elt -> 'comparable) -> 'elt list -> 'elt option

val remove : predicate:('elt -> bool) -> 'elt list -> 'elt list

val rev_map : f:('elt -> 'im) -> 'elt list -> 'im list
val map : f:('elt -> 'im) -> 'elt list -> 'im list

val rev_flatten : 'elt list list -> 'elt list
val flatten : 'elt list list -> 'elt list

val bind : 'elt list -> ('elt -> 'im list) -> 'im list

val rev_filter : predicate:('elt -> bool) -> 'elt list -> 'elt list
val filter : predicate:('elt -> bool) -> 'elt list -> 'elt list


val rev_partition :
  predicate:('elt -> bool) -> 'elt list -> 'elt list * 'elt list
val partition :
  predicate:('elt -> bool) -> 'elt list -> 'elt list * 'elt list

val map2 :
  ?on_left_remains:('left list -> 'res list)
  -> ?on_right_remains:('right list -> 'res list)
  -> f:('left -> 'right -> 'res)
  -> 'left list -> 'right list -> 'res list


val rev_take : n:int -> 'elt list -> 'elt list
val take : n:int -> 'elt list -> 'elt list

val drop : n:int -> 'elt list -> 'elt list

val cut : n:int -> 'elt list -> ('elt list * 'elt list)


val insert_nth : n:int -> 'elt -> 'elt list -> 'elt list

val remove_nth : n:int -> 'elt list -> 'elt list


val range : int -> int -> int list

val index : 'elt list -> (int * 'elt) list

val shuffle : 'elt list -> 'elt list

module Monadic : sig
    val (>>=) : 'elt list -> ('elt -> 'im list) -> 'im list
    val return : 'elt -> 'elt list
end


module MakeComparableList :
  functor (Ord : MoreModules.OrderedType) ->
    MoreModules.OrderedType
  with type t = Ord.t list
