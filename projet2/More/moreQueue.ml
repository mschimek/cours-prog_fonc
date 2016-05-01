type 'elt t = 'elt list * 'elt list

let empty = ([],[])
let is_empty (left,right) = left = [] && right = []


let add_left elt (prefix,suffix) = (elt::prefix,suffix)

let add_right (prefix,suffix) elt  = (prefix,elt::suffix)

let reverse_to_left (left,right) =
  (List.rev (List.rev_append left right), [])

let reverse_to_right (left,right) =
  ([], List.rev (List.rev_append right left))

let rev (left,right) = (right,left)

let rec observe_left = function
  | (head::tail,right) -> Some (head,(tail,right))
  | ([],[]) -> None
  | queue -> observe_left (reverse_to_left queue)

let rec observe_right = function
  | (left,head::tail) -> Some ((left,tail),head)
  | ([],[]) -> None
  | queue -> observe_right (reverse_to_right queue)

let map ~f (left,right) = (MoreList.map ~f left, MoreList.map ~f right)

let fold_left ~f init (left,right) =
  List.fold_left f init left
  |> fun res -> List.fold_left f res (List.rev right)

let fold_right ~f (left,right) init =
  let flip f x y = f y x in
  List.fold_left (flip f) init right
  |> fun res -> List.fold_left (flip f) res (List.rev left)

let filter ~predicate (left,right) =
  ( MoreList.filter ~predicate left,
    MoreList.filter ~predicate left)

let append : 'elt t -> 'elt t -> 'elt t = fun left right ->
  let (all_left,_) = reverse_to_left left in
  let (_,all_right) = reverse_to_right right in
  (all_left,all_right)

let to_rev_list queue =
  fold_left ~f:(fun list elt -> elt::list) [] queue

let flatten (left,right) =
  (left
   |> MoreList.map ~f:rev
   |> MoreList.map ~f:to_rev_list
   |> MoreList.flatten,
   right
   |> MoreList.map ~f:to_rev_list
   |> MoreList.flatten
  )

let bind queue fct =
  map ~f:fct queue |> flatten


