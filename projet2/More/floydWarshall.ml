
module type Semiring = sig
  type t
  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
end

module Make =
  functor (Ring : Semiring) ->
  functor (Elt : Set.OrderedType) ->
  functor (S : Set.S with type elt = Elt.t) ->
struct
  open Ring

  module Pair = struct
    type t = Elt.t * Elt.t
    let compare = MorePerv.compose_compare Elt.compare Elt.compare
  end
  
  module PMap = MoreMap.Make(Pair)
  

  type closure = S.elt -> S.elt -> Ring.t option
  
  let link all_pairs source destination =
    PMap.find (source,destination) all_pairs 

  
  let fold_pairs ~f set init =
    let fold fct = S.fold fct set in
    init |>
    fold @@ fun src ->
    fold @@ fun dst -> f src dst

  let map_add_if_some key assoc map =
    match assoc with
    | None -> map
    | Some assoc -> PMap.add key assoc map

  let sum_option option1 option2 =
    match option1, option2 with
    | None, _ -> option2
    | _, None -> option1
    | Some opt1, Some opt2 -> Some (opt1+opt2)
  


  let init_all_pairs ~d graph =
    let f src dst pmap =
      map_add_if_some (src,dst) (d src dst) pmap
    in 
    fold_pairs ~f graph PMap.empty


  let fw_single_round set new_vertex all_pairs  =
    let f src dst pmap =
      let old_value = PMap.find (src,dst) pmap in
      let new_value =
        let open MoreOption.Infix in
        PMap.find (src,new_vertex) pmap >>= fun prefix ->
        PMap.find (new_vertex,dst) pmap >>= fun suffix ->
        Some (prefix * suffix)
      in
      let combined_value = sum_option old_value new_value in
      map_add_if_some (src,dst) combined_value pmap
    in      
    fold_pairs ~f set all_pairs


  let transitive_closure ~d set =
    S.fold
      (fw_single_round set) 
      set
      (init_all_pairs ~d set)
  |> link

  
end

  
