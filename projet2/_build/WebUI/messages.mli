type message = string Tree.non_empty_tree

val empty_messages : Dom_html.divElement Js.t -> unit
  
val format : message -> Dom_html.divElement Js.t 

val update_messages : Dom_html.divElement Js.t -> message -> unit
