type 'elt bintree =
  | Leaf
  | Node of 'elt bintree * 'elt * 'elt bintree

val empty : 'elt bintree

val singleton : 'elt -> 'elt bintree

val catamorphism :
  on_leaf:'res ->
  on_node:('res -> ('elt bintree * 'elt * 'elt bintree) -> 'res -> 'res) ->
  'elt bintree -> 'res


val height : 'elt bintree -> int

val nbr_of_nodes : 'elt bintree -> int

val map : f:('elt -> 'im) -> 'elt bintree -> 'im bintree

val infix_order : 'elt bintree -> 'elt list

val prefix_order : 'elt bintree -> 'elt list

val list_of_edges : 'elt bintree -> ('elt * 'elt) list
