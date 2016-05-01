type 'value m =
  { apply : 'result . ('value -> 'result) -> 'result }

let (@@) f g x = f (g x)

let return value = { apply = fun f -> f value }

let bind cont fct =
  { apply = fun f -> cont.apply (fun value -> (fct value).apply f) }

module Infix = struct
  let (>>=) = bind
  let (>>) unit_cont cont2 = unit_cont >>= fun () -> cont2
end

let run_with_continuation cont fct = cont.apply fct 

let id x = x
let run cont = run_with_continuation cont id
