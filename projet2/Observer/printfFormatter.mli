type 'return t = 'return PrintfEmulation.Computation.t


module Make : functor (Value : MoreModules.PrintableType) ->
sig

  module Formatter : sig
    type format
    val send : format
    val (++) : format -> string -> format
    val (+>) : format -> Value.t -> format 
  end

  val msg : Formatter.format -> unit t 
  
end
