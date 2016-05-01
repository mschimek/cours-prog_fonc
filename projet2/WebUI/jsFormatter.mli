
module Make : functor (Value : MoreModules.DrawableType) ->
sig

  module Formatter : sig
    type format
    val send : format
    val (++) : format -> string -> format
    val (+>) : format -> Value.t -> format
  end

  val msg : Formatter.format -> JsContext.effect



end
