module Computation : BasicTypes.Computation
  with type effect = PrintfContext.effect
   and type 'result t = (PrintfContext.effect, 'result) Emulator.computation

module Emulator : BasicTypes.Emulator
  with type context = PrintfContext.context
   and type 'result t = (PrintfContext.effect, 'result) Emulator.computation


