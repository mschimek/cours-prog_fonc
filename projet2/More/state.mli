type ('value, 'state) m = 'state -> ('value * 'state)

val bind :
  ('init, 'state) m -> ('init -> ('result, 'state) m) -> ('result, 'state) m

val return : 'value -> ('value,'state) m

val get : ('state, 'state) m

val set : 'state -> (unit, 'state) m

val update_state : ('state -> 'state) -> (unit , 'state) m


val map : f:('result -> 'image) -> ('result,'state) m -> ('image,'state) m

val apply : f:('state -> 'state) -> (unit,'state) m

val repeat : n:int -> (unit, 'state) m -> (unit, 'state) m

val lift : ('state -> 'result) -> ('result,'state) m 

module Infix : sig
  val (>>=) : 
    ('init, 'state) m -> ('init -> ('result, 'state) m) -> ('result, 'state) m
  val (>>) :
    (unit, 'state) m -> ('result, 'state) m -> ('result, 'state) m
end
    
