module type S =
sig
  include Map.S
            
  val unsafe_find : key -> 'assoc t -> 'assoc
    
  val find : key -> 'assoc t -> 'assoc option
end

module Make = functor (Key : MoreModules.OrderedType) ->
struct

  include Map.Make(Key)

  let unsafe_find = find 
  
  let find key map =
    if mem key map then Some (unsafe_find key map)
    else None

end
