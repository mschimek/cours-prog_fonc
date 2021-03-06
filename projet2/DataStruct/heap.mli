module Make :
  functor (Key : MoreModules.OrderedAndPrintableType) ->
  functor (Elt : MoreModules.PrintableType) ->
  sig
    open PrintfEmulation
    
    type key = Key.t
    type elt = Elt.t
    type t
    val empty : t
    val insert : key -> elt -> t -> t Computation.t
    val merge : t -> t -> t Computation.t
    val minimum : t -> elt option
    val minimum_key : t -> key option
    val remove_minimum : t -> t Computation.t
  end
