let create = ImprovedCanvas.create

(* events *)

module Canvas = ImprovedCanvas.Monadic
                  
let resize_action event = Canvas.render

  
let zoom_action (mouse_event,(dx,dy)) =
  if not (Js.to_bool (mouse_event##shiftKey)) then
    State.return ()
  else
    let open State.Infix in
    ( if dx > 0 then State.repeat ~n:dx Canvas.zoom_out
      else State.repeat ~n:(-dx) Canvas.zoom_in
    ) >> Canvas.render

let translate_action from_vec is_last_move mouse_event =
  let open State.Infix in
  Canvas.get_vector_of_event mouse_event >>= fun to_vec ->
  Canvas.translate ~from:to_vec from_vec >>
  Canvas.render >>
  State.return is_last_move





let zoom_thread = Canvas.attach Lwt_js_events.mousewheel zoom_action


let translate_move_thread from_vec =
  Canvas.attach Lwt_js_events.mousemove (translate_action from_vec true)


let translate_up_thread from_vec widget =
  let action =  translate_action from_vec false in 
  Lwt.pick
    [ Canvas.attach Lwt_js_events.mouseup action widget;
      Canvas.attach Lwt_js_events.mouseout action widget
    ]


let mouse_down_thread  =
  Canvas.attach Lwt_js_events.mousedown (Canvas.get_vector_of_event)

let rec move_looping_thread from_vec (is_still_moving,widget) =
  let open Lwt in
  if is_still_moving then 
    pick [
      translate_up_thread from_vec widget;
      translate_move_thread from_vec widget
    ] >>= move_looping_thread from_vec
  else return ((),widget)

let translate_thread widget =
  let open Lwt in
  mouse_down_thread widget >>= fun (from_vec,widget) ->
  move_looping_thread from_vec (true,widget)
  

let redraw_thread =
  Canvas.attach (fun _ -> Lwt_js_events.onresize ()) resize_action


let start_event_handlers widget =
  let rec loop ((),widget) =
    let open Lwt in
    pick
      [ zoom_thread widget;
        translate_thread widget;
        redraw_thread widget;
      ]
    >>= loop
  in
  Lwt.async (fun () -> loop (Canvas.render widget))
  
