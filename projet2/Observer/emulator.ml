type ('effect,'result) computation =
  | Sequence :
       ('effect,'init) computation
       * ('init -> ('effect, 'return) computation)
    -> ('effect,'return) computation
  | Nest :
       'effect
       * 'effect
       * ('effect,'return) computation
    -> ('effect,'return) computation
  | Pay : ('effect,unit) computation
  | Observe : 'effect -> ('effect,unit) computation
  | Return : 'return -> ('effect,'return) computation




module Make =
  functor (Context : BasicTypes.ObservationContext) ->
struct

  
  module Computation =
  struct
    type effect = Context.effect
    open Context
    type 'any t = (effect, 'any) computation
                          
    let bind arg funct = Sequence (arg,funct)
	
    let nest
        ?(before = Context.no_effect)
        ?(after = Context.no_effect)
        comp =
      Nest (before, after, comp)
    
    let return result = Return result

    let pay = Pay

    let observe effect = Observe effect


    module Infix = struct
      let (>>=) = bind
      let (>>) unit continuation = unit >>=  fun () -> continuation
    end

  end

  module Emulator =
  struct
    open Computation.Infix                  

    type context = Context.context
    type 'return t = (Context.effect, 'return) computation
    
    type _ stack =
      | Empty : ('any -> 'any) stack
      | Cons :
          ('init -> (Computation.effect,'mid) computation)
          * ('mid -> 'final) stack
        -> ('init -> 'final) stack

    type 'result run =
      | Exec :
          (Computation.effect, 'inter) computation
          * ('inter -> 'result) stack
        -> 'result run

    type 'return result =
      | Done of 'return
      | More of 'return run (* has applied an effect *)
      | Free of 'return run (* no effect happened *)

    let init_run computation =
      Exec (computation, Empty)


    let step :
      type return . return run -> context -> (return result * context) =
      fun run context ->
        match run with
        | Exec (Return res, Empty) ->
	  (Done res,
           context)
        | Exec (Return mid, Cons (next,stack)) ->
	  (Free (Exec (next mid, stack)), context)
        | Exec (Pay, stack) ->
          (More (Exec (Return (), stack)), context)
        | Exec (Sequence (init,func), stack) ->
	  (Free (Exec (init, Cons (func, stack))), context)
        | Exec (Observe effect,stack) ->
	  (Free (Exec (Return (), stack)), Context.apply effect context)
        | Exec (Nest (opening,closing,in_between), stack) ->
	  let do_close mid =  Observe closing >> Return mid in
          ( Free (Exec (in_between, Cons (do_close,stack))),
            Context.apply opening context)


    let execute computation init_context =
      let rec loop run context =
        match step run context with
        | (Done result, final_context) -> (result,final_context)
        | (More exec, new_context)
        | (Free exec, new_context) -> loop exec new_context
      in loop (init_run computation) init_context

  end
end
