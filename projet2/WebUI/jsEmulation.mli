module Computation : sig
  include BasicTypes.Computation
    with type effect = JsContext.effect
     and type 'result t = (JsContext.effect, 'result) Emulator.computation

end


module Emulator : BasicTypes.Emulator
  with type context = JsContext.context
   and type 'result t = (JsContext.effect, 'result) Emulator.computation
             
