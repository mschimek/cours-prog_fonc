include BasicTypes.ObservationContext
  
val init_context : unit -> context

val increment : effect
val decrement : effect


val end_line : effect
  
val msg : string -> effect


module AddDrawable : functor (Value : MoreModules.DrawableType) ->
sig
  val show : ?width:int -> ?height:int -> Value.t -> effect
end
