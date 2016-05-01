
module Make = functor (Value : MoreModules.DrawableType) ->
struct


  module Draw = JsContext.AddDrawable(Value)
  
  module Formatter = struct
    type item =
      | String of string
      | Image of Value.t
    type format = item list
    let send = []
    let (++) items text = String text :: items 
    let (+>) items image = Image image :: items 
  end
  open Formatter
      
  let item_to_effect = function
    | String text -> JsContext.msg text
    | Image value -> Draw.show value

  let collapse_strings items =
    let rec loop item_accu string_accu = function
      | String str :: items ->
        loop item_accu (str::string_accu) items
      | items when string_accu <> [] ->
        let big_string = String.concat "" string_accu in
        loop (String big_string :: item_accu) [] items
      | Image img :: items ->
        loop (Image img :: item_accu) [] items
      | [] -> item_accu
    in loop [] [] items
      
  let msg items =
    items
    |> collapse_strings
    |> MoreList.map ~f:item_to_effect
    |> List.fold_left JsContext.compose JsContext.no_effect
    |> fun effect -> JsContext.compose effect JsContext.end_line


end
