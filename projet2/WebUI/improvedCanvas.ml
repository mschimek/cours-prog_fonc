type t =
  { canvas : Dom_html.canvasElement Js.t;
    image : Vg.image;
    box : Gg.box2;
    size : Gg.v2;
  }



open Gg
open Vg

let canvas_of t = t.canvas

let set_image image t =
  { t with image }

let scale_box ~x ~y box =
  let center = Box2.mid box in
  let size = Box2.size box in
  let new_size = V2.mul size (V2.v x y) in
  let new_origin = V2.sub center (V2.smul 0.5 new_size) in
  Box2.v new_origin new_size


let adapt_box_to_size size box =
  let size_aspect = V2.x size /. V2.y size in
  let box_aspect = V2.x (Box2.size box) /. V2.y (Box2.size box) in
  if size_aspect > box_aspect then
    scale_box ~x:(size_aspect /. box_aspect) ~y:1. box
  else
    scale_box ~x:1. ~y:(box_aspect /. size_aspect) box 

  
let set_box box t = { t with box = adapt_box_to_size t.size box }

    
let create ~canvas ~image ~box ~size =
  set_box box { canvas; image; box; size }



let zoom ?(factor=1.25) widget =
  { widget with
    box = scale_box ~x:factor ~y:factor widget.box
  }
    

let zoom_in = zoom ~factor:0.8
let zoom_out = zoom ~factor:1.25



let get_vector_of_event mouse_event widget =
  let rect = widget.canvas##getBoundingClientRect () in
  let (x_pix,y_pix) =
    (float (mouse_event##clientX) -. rect##left,
     float (mouse_event##clientY) -. rect##top)
  in
  let (x_max,y_max) =
    (float (widget.canvas##width),
     float (widget.canvas##height)
    )
  in
  let (x_max,y_max) =
    (Js.Optdef.case (rect##width) (fun () -> x_max) (fun fl -> fl),
     Js.Optdef.case (rect##height) (fun () -> y_max) (fun fl -> fl)
    )
  in
  let normalized = V2.v (x_pix /. x_max) ((y_max -. y_pix) /. y_max) in
  Box2.size widget.box
  |> V2.mul normalized
  |> V2.add (Box2.o widget.box)



let translate ~from:point to_position widget =
  let vec = V2.sub to_position point in
  { widget with
    box = Box2.move vec widget.box
  }


let render widget =
  let renderer = Vgr.create (Vgr_htmlc.target widget.canvas) `Other in
  let box = adapt_box_to_size widget.size widget.box in
  ignore (Vgr.render renderer (`Image (widget.size, box, widget.image)));
  ignore (Vgr.render renderer `End);
  ()



(* events *)

let mouse_move_up_callback ~on_move ~on_stop init_value widget =
  let open Lwt in 
  let rec looping_thread (value,widget) =
    Lwt.pick
      [ Lwt_js_events.mouseup widget.canvas >>= (fun ev -> return (false,ev));
        Lwt_js_events.mouseout widget.canvas >>= (fun ev -> return (false,ev));
        Lwt_js_events.mousemove widget.canvas >>= (fun ev -> return (true,ev))
      ]
    >>= fun (is_moving, event) ->
    if is_moving then
      Lwt.return (on_move event value widget) >>= looping_thread
    else Lwt.return (on_stop event value widget)
  in 
  looping_thread (init_value,widget)
    
let mouse_down_move_up_callback ~on_down ~on_move ~on_stop widget =
  let open Lwt in 
  Lwt_js_events.mousedown widget.canvas >>= fun mouse_event ->
  Lwt.return (on_down mouse_event widget) >>= fun (init_value, widget) ->
  mouse_move_up_callback ~on_move ~on_stop init_value widget




(* Monadic *)

module Monadic =
struct

  type 'result action = ('result, t) State.m
  
  let get_canvas = State.lift canvas_of

  let set_box box = State.apply ~f:(set_box box)
      
  let set_image image = State.apply ~f:(set_image image)
      
  let zoom ?factor = State.apply ~f:(zoom ?factor)
      
  let zoom_in = State.apply ~f:zoom_in

  let zoom_out = State.apply ~f:zoom_out

  let translate ~from to_vec = State.apply ~f:(translate ~from to_vec)

  let render = State.lift render 
  
  let get_vector_of_event event =
    let open State.Infix in
    State.get >>= fun widget ->
    State.return (get_vector_of_event event widget)


  let attach event_descr make_action widget =
    let open Lwt in 
    event_descr widget.canvas >>= fun event ->
    let modified = make_action event widget in
    Lwt.return (modified)

end
