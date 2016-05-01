include BasicTypes.ObservationContext

(* An effect that increases the indentation level *)
val increment : effect
  
(* An effect that decreases the indentation level *)
val decrement : effect
  
(*  An effect that prints a given message after indentation. *)
val msg : string -> effect
  
(* initialize a context, 
   take as argument the channel where to output the traces 
*)
val init_context : out_channel -> context



module AddPrintable : functor (Value : MoreModules.PrintableType) ->
sig
  
  (* An effect that prints the value *)
  val show : Value.t -> effect
    
end  
