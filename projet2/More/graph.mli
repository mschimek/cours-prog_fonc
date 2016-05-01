type vertex = int
type arc = vertex * vertex 
type t
  
module VSet : Set.S with type elt = vertex
module VMap : MoreMap.S with type key = vertex
module ESet : Set.S with type elt = arc 
module EMap : MoreMap.S with type key = arc
                               


val is_empty : t -> bool

val is_vertex : vertex -> t -> bool

val has_arc : src:vertex -> dst:vertex -> t -> bool

val order : t -> int

val vertices : t -> VSet.t

val delta_in : vertex -> t -> ESet.t

val delta_out : vertex -> t -> ESet.t 

val in_neighbours : vertex -> t -> VSet.t

val out_neighbours : vertex -> t -> VSet.t

val fold_vertices : f:(vertex -> 'accu -> 'accu) -> t -> 'accu -> 'accu

val fold_arcs : f:((vertex * vertex) -> 'accu -> 'accu) -> t -> 'accu -> 'accu



val empty : t

val add_vertex : vertex -> t -> t


val add_arc : src:vertex -> dst:vertex -> t -> t

val add_edge : vertex -> vertex -> t -> t


val remove_edge : vertex -> vertex -> t -> t

val remove_arc : src:vertex -> dst:vertex -> t -> t

val remove_vertex : vertex -> t -> t 



