type t

val create :
  canvas:Dom_html.canvasElement Js.t
  -> image:Vg.image
  -> box:Gg.box2
  -> size:Gg.size2
  -> t


(* direct functions *)

val canvas_of : t -> Dom_html.canvasElement Js.t

val set_box : Gg.box2 -> t -> t 
val set_image : Vg.image -> t -> t

val zoom : ?factor:float -> t -> t 
val zoom_in : t -> t 
val zoom_out : t -> t 

val translate : from:Gg.p2 -> Gg.p2 -> t -> t 


val render : t -> unit 

val get_vector_of_event : Dom_html.mouseEvent Js.t -> t -> Gg.p2



(* events *)

val mouse_move_up_callback :
  on_move:(Dom_html.mouseEvent Js.t -> 'value -> t -> ('value * t))
  -> on_stop:(Dom_html.mouseEvent Js.t -> 'value -> t ->  ('result * t))
  -> 'value
  -> t
  -> ('result * t) Lwt.t

val mouse_down_move_up_callback :
  on_down:(Dom_html.mouseEvent Js.t -> t -> ('value * t) )
  -> on_move:(Dom_html.mouseEvent Js.t -> 'value -> t -> ('value * t))
  -> on_stop:(Dom_html.mouseEvent Js.t -> 'value -> t -> ('result * t))
  -> t 
  -> ('result * t) Lwt.t
    
(* state monad functions *)

module Monadic :
sig
  type 'result action = ('result,t) State.m

      
  val get_canvas : Dom_html.canvasElement Js.t action

  val set_box : Gg.box2 -> unit action
  val set_image : Vg.image -> unit action

  val zoom : ?factor:float -> unit action

  val zoom_in : unit action
  val zoom_out : unit action

  val translate : from:Gg.v2 -> Gg.v2 -> unit action

  val render : unit action


  val get_vector_of_event : Dom_html.mouseEvent Js.t -> Gg.p2 action
  
  val attach :
    ( Dom_html.canvasElement Js.t -> 'event Lwt.t )
    -> ('event -> 'result action)
    -> t -> ('result * t ) Lwt.t 

end
    

    
