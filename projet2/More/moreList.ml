let rec find ~predicate = function
  | head::tail when predicate head -> Some head
  | _::tail -> find predicate tail
  | [] -> None


let remove ~predicate list =
  let rec loop accu = function
    | head::tail when predicate head -> List.rev_append accu tail
    | head::tail -> loop (head::accu) tail
    | [] -> List.rev accu
  in loop [] list



let rev_map ~f list =
  let rec loop accu = function
    | head::tail -> loop (f head :: accu) tail
    | [] -> accu
  in
  loop [] list

let map ~f list = list |> rev_map ~f |> List.rev


let rev_flatten list =
  let rec loop accu = function
    | (head::tail)::others -> loop (head::accu) (tail::others)
    | []::tail -> loop accu tail
    | [] -> accu
  in loop [] list


let flatten list = list |> rev_flatten |> List.rev


let bind list f = list |> List.map f |> flatten

let rev_filter ~predicate list = 
  let rec loop accu = function
    | head::tail when predicate head -> loop (head::accu) tail
    | _::tail -> loop accu tail
    | [] -> accu
  in loop [] list

let filter ~predicate list = list |> rev_filter ~predicate |> List.rev


let rev_partition ~predicate list =
  let rec loop accu_true accu_false = function
    | head::tail when predicate head ->
      loop (head::accu_true) accu_false tail
    | head::tail ->
      loop accu_true (head::accu_false) tail
    | [] -> (accu_true, accu_false)
  in loop [] [] list

let partition ~predicate list =
  let (yes_items,no_items) = rev_partition ~predicate list in
  (List.rev yes_items, List.rev no_items)


let map2
    ?(on_left_remains = fun any -> [])
    ?(on_right_remains = fun any -> [])
    ~f
  =
  let rec loop accu left right =
    match left, right with
    | [], [] -> List.rev accu 
    | [], _ -> List.rev_append accu (on_right_remains right)
    | _, [] -> List.rev_append accu (on_left_remains left)
    | l1::ltail, r1::rtail ->
      loop (f l1 r1 :: accu) ltail rtail
  in
  fun left right -> loop [] left right


let rev_take ~n list =
  let rec loop accu current_pos = function
      | _ when current_pos >= n -> accu
      | head::tail -> loop (head::accu) (current_pos+1) tail
      | [] -> accu
  in loop [] 0 list

let take ~n list = list |> rev_take ~n |> List.rev

let rec drop ~n = function
  | list when n <= 0 -> list
  | head::tail -> drop ~n:(n-1) tail
  | [] -> []


let cut ~n list =
  let rec loop rev_prefix current_pos = function
    | suffix when current_pos >= n -> (rev_prefix,suffix)
    | head::tail -> loop (head::rev_prefix) (current_pos+1) tail
    | [] -> (rev_prefix,[])
  in loop [] 0 list

let insert_nth ~n elt list =
  let rev_prefix,suffix = cut ~n list in
  List.rev_append rev_prefix (elt :: suffix)

let remove_nth ~n list =
  let (rev_prefix,suffix) = cut ~n list in
  match suffix with
  | nth::tail -> List.rev_append rev_prefix tail
  | [] -> List.rev rev_prefix


let range a b =
  let rec loop accu current_index =
    if current_index < a then accu
    else loop (current_index :: accu) (current_index - 1)
  in loop [] b


let index list =
  let add_item (index,previous_items) item =
    (index+1, (index,item)::previous_items)
  in
  let (length, reversed_indexed) =
    List.fold_left add_item (0,[]) list
  in
  List.rev reversed_indexed




let find_minimum ~objective = function
  | [] -> None
  | head::tail ->
    let min ((elt1,value1) as arg1) ((elt2,value2) as arg2) =
      if value1 < value2 then arg1 else arg2
    in
    tail
    |> List.map (fun elt -> (elt,objective elt))
    |> List.fold_left min (head,objective head)
    |> fst
    |> fun best -> Some best




let random_predicate item = Random.bool ()
let rec shuffle = function
  | [] -> []
  | [single] -> [single]
  | list ->
    let (left,right) = rev_partition ~predicate:random_predicate list in
    List.rev_append (shuffle left) (shuffle right)
      

module Monadic = struct
    let (>>=) = bind
    let return elt = [elt]
end



module MakeComparableList =
  functor (Ord : MoreModules.OrderedType) ->
  struct
    type t = Ord.t list
    let rec compare list1 list2 =
      match list1, list2 with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | head1::tail1, head2::tail2 ->
        MorePerv.compose_compare Ord.compare compare (head1,tail1) (head2,tail2)
  end
          
        



          
