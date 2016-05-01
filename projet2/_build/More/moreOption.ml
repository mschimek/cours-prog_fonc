let bind option fct = match option with
  | None -> None
  | Some elt -> fct elt

let default def_value = function
  | None -> def_value
  | Some thing -> thing

let map f = function
  | None -> None
  | Some thing -> Some (f thing)

let guard predicate = function
  | None -> None
  | Some wrong when predicate wrong -> None
  | ok -> ok

let do_if_defined option fct = match option with
  | Some thing -> fct thing
  | None -> ()

let of_option_list list =
  if List.for_all (fun elt -> elt <> None) list then
    Some (MoreList.bind list (function None -> [] | Some e -> [e]))
  else None

let product opt1 opt2 = match opt1, opt2 with
  | Some elt1, Some elt2 -> Some (elt1, elt2)
  | _ -> None


let flatten = function
  | Some e -> e
  | None -> None

module Infix = struct
  let (>>=) = bind
end
