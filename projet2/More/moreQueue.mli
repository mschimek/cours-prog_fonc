type 'elt t

val empty : 'elt t
val is_empty : 'elt t -> bool

val add_right : 'elt t -> 'elt -> 'elt t
val add_left : 'elt -> 'elt t -> 'elt t

val observe_left : 'elt t -> ('elt * 'elt t) option
val observe_right : 'elt t -> ('elt t * 'elt) option

val map : f:('elt -> 'im) -> 'elt t -> 'im t
val fold_left : f:('accu -> 'elt -> 'accu) -> 'accu -> 'elt t -> 'accu
val fold_right : f:('elt -> 'accu -> 'accu) -> 'elt t -> 'accu -> 'accu
val filter : predicate:('elt -> bool) -> 'elt t -> 'elt t
val flatten : 'elt t t -> 'elt t
val bind : 'elt t -> ('elt -> 'res t) -> 'res t 
