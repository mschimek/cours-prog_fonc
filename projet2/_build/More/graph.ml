module Vertex = struct
  type t = int
  let compare = compare
end
module Arc = struct
  type t = int * int
  let compare = compare
end

module VSet = Set.Make(Vertex)
module VMap = MoreMap.Make(Vertex)
module ESet = Set.Make(Arc)
module EMap = MoreMap.Make(Arc)

type vertex = Vertex.t 
type arc = Arc.t 

type t =
  {
    vertices : VSet.t;
    in_adjacency : ESet.t VMap.t;
    out_adjacency : ESet.t VMap.t
       
  }



(* *** basic usage *** *)

let is_empty { vertices } = VSet.is_empty vertices


let order graph = VSet.cardinal graph.vertices

let vertices graph = graph.vertices
                       
let is_vertex vertex graph =
  VSet.mem vertex graph.vertices



let delta_in vertex graph = 
  VMap.find vertex graph.in_adjacency
  |> MoreOption.default ESet.empty

let delta_out vertex graph =
  VMap.find vertex graph.out_adjacency
  |> MoreOption.default ESet.empty


let has_arc ~src ~dst graph =
  ESet.exists (fun (u,v) -> v = dst) (delta_out src graph)

let are_adjacent u v graph =
  has_arc ~src:u ~dst:v graph
  || has_arc ~src:v ~dst:v graph 

let in_neighbours vertex graph =
  ESet.fold (fun (src,dst) -> VSet.add dst) (delta_out vertex graph) VSet.empty

let out_neighbours vertex graph =
  ESet.fold (fun (src,dst) -> VSet.add src) (delta_in vertex graph) VSet.empty


let fold_vertices ~f graph init =
  VSet.fold f graph.vertices init


let fold_arcs ~f:fct graph init =
  VMap.fold
    (fun src -> ESet.fold fct)
    graph.out_adjacency
    init



(* *** building a graph *** *)

let empty =
  { 
    vertices = VSet.empty;
    in_adjacency = VMap.empty;
    out_adjacency = VMap.empty
  }



let add_vertex vertex graph =
  if is_vertex vertex graph then graph
  else
    {
      vertices = VSet.add vertex graph.vertices;
      in_adjacency = VMap.add vertex ESet.empty graph.in_adjacency;
      out_adjacency = VMap.add vertex ESet.empty graph.out_adjacency
          
    }

let add_adjacency ~src ~dst graph =
  let out_src = delta_out src graph in
  let in_dst = delta_in dst graph in
  { graph with
    in_adjacency =
      VMap.add dst (ESet.add (src,dst) in_dst) graph.in_adjacency;
    out_adjacency =
      VMap.add src (ESet.add (src,dst) out_src) graph.out_adjacency;
  }
  


let add_arc ~src ~dst graph =
  if has_arc ~src ~dst graph then graph
  else
    graph
    |> add_vertex src
    |> add_vertex dst
    |> add_adjacency ~src ~dst 

let add_edge source destination graph =
  graph
  |> add_arc ~src:source ~dst:destination
  |> add_arc ~src:destination ~dst:source



let remove_arc ~src ~dst graph =
  if not (has_arc ~src ~dst graph) then graph
  else
    let out_src = delta_out src graph in
    let in_src = delta_in dst graph in
    { graph with
      in_adjacency =
        VMap.add dst (ESet.remove (src,dst) in_src) graph.in_adjacency;
      out_adjacency =
        VMap.add src (ESet.remove (src,dst) out_src) graph.out_adjacency;
    }
  
let remove_edge source destination graph =
  graph
  |> remove_arc ~src:source ~dst:destination
  |> remove_arc ~src:destination ~dst:source

  
let remove_vertex vertex graph =
  if not (is_vertex vertex graph) then graph
  else
    let remove_arc (src,dst) = remove_arc ~src ~dst in
    { 
      vertices = VSet.remove vertex graph.vertices;
      in_adjacency = VMap.remove vertex graph.in_adjacency;
      out_adjacency = VMap.remove vertex graph.out_adjacency;
    }
    |> ESet.fold remove_arc (delta_in vertex graph)
    |> ESet.fold remove_arc (delta_out vertex graph)




(* *** distance-based algorithms *** *)

module ShortPath = struct
  type t = int * vertex option BinTree.bintree 
  let (+) (len1,path1) (len2,path2) =
    if len1 <= len2 then (len1,path1)
    else (len2,path2)
  and ( * ) (len1,path1) (len2,path2) =
    (len1 + len2, BinTree.(Node (path1, None, path2)))
end

module FW = FloydWarshall.Make(ShortPath)(Vertex)(VSet)

type all_shortest_paths = FW.closure

let all_pairs_shortest_paths ~d graph =
  let d src dst = match d src dst with
    | None -> None
    | Some length ->
      Some (length, BinTree.singleton (Some dst))
  in
  FW.transitive_closure ~d graph.vertices


let distance closure ~src ~dst =
  match closure src dst with
  | None -> None
  | Some (dist,bintree) -> Some dist

let shortest_path closure ~src ~dst =
  match closure ~src ~dst with
  | None -> None
  | Some (len,tree) ->
    let open MoreList.Monadic in
    let vertices =
      BinTree.infix_order tree >>= function
      | None -> []
      | Some e -> [e]
    in
    Some (src::vertices)
  
