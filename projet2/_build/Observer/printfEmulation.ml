module Pair = Emulator.Make(PrintfContext)

module Computation = Pair.Computation

module Emulator = Pair.Emulator
