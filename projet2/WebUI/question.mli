
  
type 'answer form =
  { display : Dom_html.divElement Js.t;
    get_answer : unit -> 'answer option;
  }
  
type 'answer t 


val get_int : title:Dom_html.divElement Js.t -> int t
val get_float : title:Dom_html.divElement Js.t -> float t
val get_string : title:Dom_html.divElement Js.t -> string t

val question_of_parser :
  title:Dom_html.element Js.t ->
  reader:'answer Parsec.read ->
  'answer t

val question_of_form :
  title:Dom_html.element Js.t ->
  form : 'answer form ->
  'answer t 


val pack : title:Dom_html.element Js.t -> 'answer t -> 'answer t

val conjonction :
  'left t
  -> 'right t
  -> ('left * 'right) t


val disjonction :
  title:Dom_html.element Js.t
  -> 'answer t list
  -> 'answer t

val bind : 'first t -> ('first -> 'answer t) -> 'answer t

val return : 'answer -> 'answer t

val map : f:('answer -> 'image) -> 'answer t -> 'image t 

val propose : father:Dom_html.divElement Js.t -> 'answer t -> 'answer Lwt.t


val ask_int : string -> int t
val ask_float : string -> float t
val ask_string : string -> string t

val of_list : 'answer t list -> 'answer list t 

module Infix : sig
  val (+>) : ('value -> 'answer) t -> 'value t -> 'answer t
  val ( *> ) : ('value -> 'answer) -> 'value t -> 'answer t
  val (>>=) : 'first t -> ('first -> 'second t) -> 'second t 
end
