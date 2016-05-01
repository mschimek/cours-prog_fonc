
type 'elt non_empty_tree =
 Node of 'elt * 'elt non_empty_tree list
type 'elt tree =
 | Empty
 | NonEmpty of 'elt non_empty_tree

val root : 'node non_empty_tree -> 'node

val catamorphism :
    on_empty:'res
 -> on_node:('node * 'node non_empty_tree list -> 'res list -> 'res)
 -> 'node tree -> 'res

val singleton : 'elt -> 'elt non_empty_tree


type 'elt zipped_tree

val zip : 'elt tree -> 'elt zipped_tree
val of_zip : 'elt zipped_tree -> 'elt tree

val zip_arity : 'elt zipped_tree -> int
val zip_rank : 'elt zipped_tree -> int option
val zip_current_subtree : 'elt zipped_tree -> 'elt tree
val zip_current_node : 'elt zipped_tree -> 'elt option

val is_at_top : 'elt zipped_tree -> bool
val is_at_left : 'elt zipped_tree -> bool
val is_at_right : 'elt zipped_tree -> bool
val is_at_bottom : 'elt zipped_tree -> bool

val zip_up : 'elt zipped_tree -> 'elt zipped_tree
val zip_left : 'elt zipped_tree -> 'elt zipped_tree
val zip_right : 'elt zipped_tree -> 'elt zipped_tree
val zip_down : 'elt zipped_tree -> 'elt zipped_tree

val zip_to_root : 'elt zipped_tree -> 'elt zipped_tree
val zip_to_left : 'elt zipped_tree -> 'elt zipped_tree
val zip_to_right : 'elt zipped_tree -> 'elt zipped_tree


type position =
 | Leftmost
 | Nth of int
 | Rightmost


(** [update_subtree new_subtree zipper]
   substitutes the subtree currently pointed to by [zipper]
   with [new_subtree]
*)
val update_subtree :
 f:('elt non_empty_tree -> 'elt non_empty_tree) -> 
 'elt zipped_tree -> 'elt zipped_tree

(** insert a subtree at a given position, without changing the current node *)
val insert_subtree :
 ?position:position
 -> 'elt non_empty_tree -> 'elt zipped_tree -> 'elt zipped_tree

val remove_subtree :
  ?position:position -> 'elt zipped_tree -> 'elt zipped_tree
    
(** then move up *)
val remove_current_tree :
 'elt zipped_tree -> 'elt zipped_tree
