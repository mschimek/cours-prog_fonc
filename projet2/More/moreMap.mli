module type S =
sig
  include Map.S
            
  val unsafe_find : key -> 'assoc t -> 'assoc
    
  val find : key -> 'assoc t -> 'assoc option
end

module Make : functor (Key : MoreModules.OrderedType) ->
  S with type key = Key.t 
