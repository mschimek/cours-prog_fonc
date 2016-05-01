type ('effect, 'result) computation


module Make :
  functor (Context : BasicTypes.ObservationContext) ->
  sig
    module Computation : BasicTypes.Computation
      with type effect = Context.effect
       and type 'result t = (Context.effect, 'result) computation


    module Emulator : BasicTypes.Emulator
      with type context = Context.context
       and type 'result t = (Context.effect, 'result) computation
  end

      
