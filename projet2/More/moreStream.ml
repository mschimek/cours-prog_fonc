
(* type definition *)

type 'a _stream = Nil | Cons of 'a * 'a t
and 'a t = 'a _stream Lazy.t

let empty = lazy Nil 
let is_empty stream = Lazy.force stream = Nil

exception End_of_stream


(* Views *)

let view stream = match Lazy.force stream with
  | Nil -> None
  | Cons (first,tail) -> Some (first,tail)

let first stream = match view stream with
  | None -> raise End_of_stream
  | Some (first,_) -> first
 
let tail stream = match view stream with
  | None -> raise End_of_stream
  | Some (_,tail) -> tail

let to_list stream =
  let rec with_accu accu stream = match view stream with
    | None -> List.rev accu
    | Some (first,tail) -> with_accu (first::accu) tail
  in with_accu [] stream


(* constructors *)

let rec repeat value = lazy (Cons (value, repeat value))

let rec iterate funct first_value =
  lazy (
    Cons (first_value, iterate funct (funct first_value))
  )

let rec from_generator ~gen initial =
  lazy (match gen initial with
  | None -> Nil
  | Some (next,value) -> Cons (value, from_generator ~gen next)
  )

let from_index genf = 
  let rec at_index i = lazy ( Cons (genf i, at_index (i+1)) ) in
  at_index 0

let from_list lst =
  from_generator 
    ~gen:(function
        | [] -> None
        | head::tail -> Some (tail,head)
      ) lst
    
let from_array array =
  let gen i =
    if i < Array.length array then Some (i+1,array.(i))
    else None
  in
  from_generator ~gen 0

let from_string string =
  let gen i =
    if i < String.length string then Some (i+1,string.[i])
    else None
  in
  from_generator ~gen 0
  
let from_in_channel in_chan =
  let gen () =
    try Some ((), input_char in_chan)
    with End_of_file -> None
  in
  from_generator ~gen ()


let add elt stream = lazy (Cons (elt,stream))

let rec append stream1 stream2 =
  lazy (match view stream1 with
    | None -> Lazy.force stream2
    | Some (first,tail) -> Cons (first, append tail stream2)
  )

let rec cycle stream = 
  lazy (match view stream with
  | None -> Nil
  | Some (first,tail) -> Cons (first, append tail (cycle stream))
  )


(* simple manipulations *)

let take k stream = 
  let rec with_accu k stream accu =
    if k = 0 || is_empty stream then List.rev accu
    else with_accu (k-1) (tail stream) (first stream :: accu)
  in with_accu k stream []

let rec drop k stream =
  if k = 0 || is_empty stream then stream
  else drop (k-1) (tail stream)

let kth k stream = first (drop k stream)

let break_when predicate =
  let rec with_accu accu stream = match view stream with
    | None -> (List.rev accu, stream)
    | Some (first,_) when predicate first -> (List.rev accu,stream)
    | Some (first,tail) -> with_accu (first::accu) tail
  in with_accu []

let drop_until predicate stream = snd (break_when predicate stream)

let keep_until predicate stream = fst (break_when predicate stream)

let rec find predicate stream =
  let open MoreOption.Infix in
  view stream >>= fun (first,tail) -> 
  if predicate first then Some first 
  else find predicate tail

(* Higher order functions *)

let map mapped_fct stream =
  let open MoreOption.Infix in
  let gen stream =
    view stream >>= fun (first,tail) ->
    Some (tail,mapped_fct first)
  in
  from_generator ~gen stream


let scan (++) init_value stream =
  let open MoreOption.Infix in
  let gen (current_value,stream) = 
    view stream >>= (fun (first,tail) -> 
      let next_value = current_value ++ first in 
      Some ((next_value,tail),next_value)
    )
  in from_generator ~gen (init_value,stream)


let rec filter predicate stream = lazy (
  match view stream with
  | None -> Nil
  | Some (first,tail) ->
    if predicate first then Cons (first, (filter predicate tail))
    else Lazy.force (filter predicate tail)
)

let rec partition predicate stream =
  (filter predicate stream,
   filter (fun elt -> not (predicate elt)) stream)


let rec flatten streams = lazy (
  match view streams with
  | None -> Nil
  | Some (stream,others) -> Lazy.force (append stream (flatten others))
)


(* Zip and unzip *)

let rec combine left right = lazy (
  match view left, view right with
  | None,_
  | _, None -> Nil
  | Some (firstl,taill), Some (firstr,tailr) ->
    Cons ((firstl,firstr), combine taill tailr)
)

let rec split stream = (map fst stream, map snd stream)

let map2 fct left right = map (fun (l,r) -> fct l r) (combine left right)


(* Monad *)


module Monad =
struct
  let (>>=) stream funct = flatten (map funct stream)
  let return value = add value empty
end


(* Infix operators *)

module Infix =
struct
  let (@@) = append
  let (++) = add
  let (!!) = kth
  let ( ** ) = combine
end
