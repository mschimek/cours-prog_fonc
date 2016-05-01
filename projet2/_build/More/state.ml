type  ('value, 'state) m = 'state -> ('value * 'state)

let bind f_init fct =
  fun state ->
    let (res_init,inter_state) = f_init state in
    fct res_init inter_state 

let return value = fun state -> (value, state)

let set new_state = fun old_state -> ((),new_state)

let get = fun current_state -> (current_state, current_state)

    
let update_state fct = fun old_state -> ((),fct old_state)

module Infix = struct
  let (>>=) = bind
  let (>>) fct1 fct2 = fct1 >>= fun () -> fct2
end

open Infix
    
let map ~f action = action >>= fun result -> return (f result)

let apply ~f = get >>= fun state -> set (f state)

let rec repeat ~n action =
  if n = 0 then return ()
  else action >> repeat ~n:(n-1) action


let lift fct = map ~f:fct get
