type 'elt non_empty_tree =
 Node of 'elt * 'elt non_empty_tree list

type 'elt tree =
 | Empty
 | NonEmpty of 'elt non_empty_tree



let root (Node (root,subtrees)) = root

let catamorphism ~on_empty ~on_node = function
  | Empty -> on_empty
  | NonEmpty root ->
     let rec loop (Node (root,subtrees)) =
       subtrees 
       |>List.map loop
       |> on_node (root,subtrees)
     in loop root


let height tree =
  catamorphism 
    ~on_empty:(-1)
    ~on_node:(fun _ heights -> 1 + List.fold_left max (-1) heights)
    tree


let nbr_of_nodes tree =
  catamorphism
    ~on_empty:0
    ~on_node:(fun _ orders -> List.fold_left (+) 1 orders)
    tree


let singleton elt = Node (elt,[])


type 'elt backstep = ('elt non_empty_tree list * 'elt * 'elt non_empty_tree list)
type 'elt zipped_tree =
  | EmptyZip
  | NonEmptyZip of ('elt non_empty_tree * 'elt backstep list)



let zip = function
  | Empty -> EmptyZip
  | NonEmpty node -> NonEmptyZip (node, [])

let zip_current_node = function
  | EmptyZip -> None
  | NonEmptyZip (Node (root,sons), path) ->
    Some root

let is_at_top = function
  | NonEmptyZip (_,_::_) -> false
  | _ -> true

let is_at_left = function 
  | NonEmptyZip (_,((_::_),_,_)::_) -> false
  | _ -> true

let is_at_right = function 
  | NonEmptyZip (_,(_,_,(_::_))::_) -> false
  | _ -> true
    
let is_at_bottom = function
  | NonEmptyZip (Node (node,_::_),_) -> false
  | _ -> false

let zip_up = function
  | NonEmptyZip (current,(left_siblings,father,right_siblings)::above) -> 
     let siblings = List.rev_append left_siblings (current :: right_siblings) in
     NonEmptyZip (Node (father, siblings), above)
  | any -> any

let zip_left = function
  | NonEmptyZip (current,((left::leftists),father,rights)::above) -> 
     NonEmptyZip (left, (leftists,father,current::rights)::above)
  | any -> any

let zip_right = function
| NonEmptyZip (current,(lefts,father,right::rightists)::above) ->
   NonEmptyZip (right, (current::lefts,father,rightists)::above)
| any -> any

let zip_down = function
  | NonEmptyZip (Node (current,child::children),above) ->
     NonEmptyZip (child, ([],current,children) :: above)
| any -> any


let many_zip ~stop ~toward zipper =
  let rec loop zipper =
    if stop zipper then zipper
    else loop (toward zipper)
  in
  loop zipper

let zip_to_root zipper = many_zip ~stop:is_at_top ~toward:zip_up zipper
let zip_to_left zipper = many_zip ~stop:is_at_left ~toward:zip_left zipper
let zip_to_right zipper = many_zip ~stop:is_at_right ~toward:zip_right zipper


let of_zip zipper = match zip_to_root zipper with
  | EmptyZip -> Empty
  | NonEmptyZip (root,_) -> NonEmpty root



let zip_arity = function
  | EmptyZip -> 0
  | NonEmptyZip (Node (value,children),_) -> List.length children

let zip_rank = function
  | NonEmptyZip  (_, (left_siblings,_,_)::_) -> Some (List.length left_siblings)
  | root_or_empty -> None
  
let zip_current_subtree = function
  | NonEmptyZip (subtree,above) -> NonEmpty subtree
  | EmptyZip -> Empty

type position = 
  | Leftmost
  | Nth of int
  | Rightmost
	  

let update_subtree ~f = function
  | NonEmptyZip (subtree,above) -> NonEmptyZip (f subtree, above)
  | EmptyZip -> EmptyZip


let nth_from_pos list = function
  | Leftmost -> 0
  | Rightmost -> List.length list
  | Nth i -> i

let insert_subtree ?(position=Leftmost) new_tree = function
  | NonEmptyZip (Node (current,children), above) ->
     let n = nth_from_pos children position in
     let new_children = MoreList.insert_nth ~n new_tree children in
     NonEmptyZip  (Node (current, new_children), above)
  | EmptyZip -> NonEmptyZip (new_tree, [])


let remove_subtree ?(position=Leftmost) = function
  | NonEmptyZip (Node (current, children),above) ->
     let n = nth_from_pos children  position in
     let new_children = MoreList.remove_nth ~n children in
     NonEmptyZip (Node (current, new_children), above)
  | EmptyZip -> EmptyZip
		

let remove_current_tree = function
    | NonEmptyZip (_,(left_siblings,father,right_siblings)::above) ->
       NonEmptyZip (
	   Node (father, List.rev_append left_siblings right_siblings),
	   above)
    | NonEmptyZip(_,[]) -> EmptyZip
    | EmptyZip -> EmptyZip
