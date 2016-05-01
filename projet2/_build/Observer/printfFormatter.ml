type 'return t = 'return PrintfEmulation.Computation.t

module Make = functor (Value : MoreModules.PrintableType) ->
struct

  open PrintfEmulation
  
  module Formatter = struct
    type item =
      | String of string
      | Value of Value.t
    type format = item list
    let send = []
    let (++) items text = String text :: items 
    let (+>) items value = Value value :: items 
  end
  open Formatter
      
  let item_to_string = function
    | String text -> text
    | Value value -> Value.to_string value

      
  let msg items =
    items
    |> MoreList.rev_map ~f:item_to_string
    |> String.concat ""
    |> PrintfContext.msg 
    |> Computation.observe  


end
