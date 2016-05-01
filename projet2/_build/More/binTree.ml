type 'elt bintree =
  | Leaf
  | Node of 'elt bintree * 'elt * 'elt bintree

let empty = Leaf

let is_empty tree = tree = Leaf

let singleton elt = Node (Leaf, elt, Leaf)

let catamorphism ~on_leaf ~on_node tree =
  let rec loop tree k = match tree with
  | Leaf -> k on_leaf
  | Node (left,root,right) -> 
     loop left @@ fun lres -> 
     loop right @@ fun rres -> 
     k (on_node lres (left,root,right) rres)
  in loop tree (fun id -> id)

let height tree =
  catamorphism ~on_leaf:(-1) ~on_node:(fun l _ r -> 1 + max l r) tree

let nbr_of_nodes tree =
  catamorphism ~on_leaf:0 ~on_node:(fun l _ r -> 1 + l + r) tree

let rec map ~f tree =
  catamorphism 
    ~on_leaf:Leaf 
    ~on_node: (fun l (_,root,_) r ->  Node (l, f root, r)) 
    tree

let infix_order tree =
  let rec loop tree accu = match tree with
    | Leaf -> accu
    | Node (left,root,right) ->
       accu 
       |> loop left
       |> (fun l -> root::l)
       |> loop right
  in List.rev (loop tree [])

let prefix_order tree =
  let rec loop tree accu = match tree with
    | Leaf -> accu
    | Node (left,root,right) ->
       root::accu 
       |> loop left
       |> loop right
  in List.rev (loop tree [])


let list_of_edges tree =
  let rec loop father tree accu = match tree with
    | Leaf -> accu
    | Node (left,node,right) ->
      (father,node)::accu |> loop node left |> loop node right
  in
  match tree with
  | Leaf -> []
  | Node (left,node,right) ->
    [] |> loop node left |> loop node right


      
