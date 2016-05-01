val cons : 'elt -> 'elt list -> 'elt list

val repeat : f:('any -> 'any) -> int -> 'any -> 'any


val compose : ('inter -> 'result) -> ('init -> 'inter) -> ('init -> 'result)

type 'value comparison = 'value -> 'value -> int
  
val compose_compare :
  'left comparison -> 'right comparison -> ('left * 'right) comparison
