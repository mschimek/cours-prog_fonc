let debug = true
let trace str = if debug then Printf.printf "%s%!" str 

module VSet = Graph.VSet
module VMap = Graph.VMap
module EMap = Graph.EMap
module ESet = Graph.ESet

type layout = Gg.p2 Graph.VMap.t


type t =
  { graph : Graph.t;
    layout : layout;
    widget : ImprovedCanvas.t;
    arc_properties : [`Arc] MoreImage.property list EMap.t;
    vertex_properties : [`Vertex] MoreImage.property list VMap.t;
  }


let on_graph action t =
  { t with
    graph = action t.graph
  }

let on_widget action t =
  { t with
    widget = action t.widget;
  }


let (@@) f g x = f (g x)

let get_position vertex t =
  VMap.find vertex t.layout |> MoreOption.default (Gg.P2.v 0. 0.)



let get_vertex_properties vertex t = 
  VMap.find vertex t.vertex_properties |> MoreOption.default []
    
let get_arc_properties arc t =
  EMap.find arc t.arc_properties |> MoreOption.default [] 

let set_vertex_properties new_properties vertex t =
  let properties =
    new_properties @ get_vertex_properties vertex t
  in
  { t with
    vertex_properties = VMap.add vertex properties t.vertex_properties
  }

let remove_vertex_properties remover vertex t =
  let properties =
    List.fold_left (|>) (get_vertex_properties vertex t) remover
  in
  { t with
    vertex_properties = VMap.add vertex properties t.vertex_properties
  }

let set_arc_properties new_properties arc t =
  let properties =
    new_properties @ get_arc_properties arc t
  in
  { t with
    arc_properties = EMap.add arc properties t.arc_properties
  }

let remove_arc_properties remover arc t =
  let properties =
    List.fold_left (|>) (get_arc_properties arc t) remover
  in
  { t with
    arc_properties = EMap.add arc properties t.arc_properties
  }


let selected_vertex_properties = 
  let open MoreImage.Properties in
    [ radius 0.15;
      linewidth 0.03;
      filling_color Gg.Color.red
    ]

let set_vertex_selected =
  set_vertex_properties selected_vertex_properties

let remove_vertex_selected  =
  let open MoreImage.Properties in
  remove_vertex_properties 
    [ remove_radius;
      remove_linewidth;
      remove_filling_color
    ]


let moving_vertex_properties = 
  let open MoreImage.Properties in
  [ drawing_color (Gg.Color.gray 0.6);
    filling_color (Gg.Color.gray 0.6)
  ]

let set_vertex_moving =
  set_vertex_properties moving_vertex_properties

let remove_vertex_moving =
  let open MoreImage.Properties in
  remove_vertex_properties
    [ remove_drawing_color;
      remove_filling_color
    ]


let set_vertex_hovered vertex t =
  let open MoreImage.Properties in
  let fill_color =
    match get_filling_color (get_vertex_properties vertex t) with
    | Some color -> Gg.Color.gray 0.6
    | None -> Gg.Color.gray 0.6
  in
  set_vertex_properties
    [ linewidth 0.002;
      filling_color fill_color 
    ]
    vertex t

let remove_vertex_hovered =
  let open MoreImage.Properties in
  remove_vertex_properties
    [ remove_linewidth;
      remove_filling_color
    ]



let selected_arc_properties = 
  let open MoreImage.Properties in
  [ drawing_color Gg.Color.red;
    filling_color Gg.Color.red;
    linewidth 0.02;
  ]

let set_arc_selected =
  set_arc_properties selected_arc_properties

let remove_arc_selected =
  let open MoreImage.Properties in
  remove_arc_properties
    [ remove_drawing_color;
      remove_filling_color;
      remove_linewidth
    ]

let moving_arc_properties =
  let open MoreImage.Properties in
  [ drawing_color (Gg.Color.gray 0.6);
    filling_color (Gg.Color.gray 0.6)
  ]

let set_arc_moving =
  set_arc_properties moving_arc_properties

let remove_arc_moving =
  let open MoreImage.Properties in
  remove_arc_properties
    [ remove_drawing_color;
      remove_filling_color
    ]

let set_arc_hovered arc t =
  let open MoreImage.Properties in
  let lw = get_linewidth (get_arc_properties arc t) in
  set_arc_properties
    [ linewidth (1.5 *. lw);
    ]
    arc t 

let remove_arc_hovered =
  let open MoreImage.Properties in
  remove_arc_properties
    [ remove_linewidth;
    ]




let render t =
  MoreImage.draw_graph
    ~vertex_properties:(fun v -> get_vertex_properties v t)
    ~arc_properties:(fun v -> get_arc_properties v t)
    ~layout:(fun v -> get_position v t)
    t.graph 


let redraw ?(more_arcs=[]) ?(more_vertices=[]) t =
  trace "redraw";
  let draw_arc (src_point,dst_point) =
    MoreImage.draw_arc ~properties:moving_arc_properties src_point dst_point
  in
  let draw_vertex point =
    MoreImage.draw_vertex ~properties:moving_vertex_properties point
  in
  let (img, box) = render t in
  let image =
    img
    |> List.fold_right draw_arc more_arcs
    |> List.fold_right draw_vertex more_vertices
  in
  let t = 
    { t with widget = ImprovedCanvas.set_image image t.widget }
  in
  ImprovedCanvas.render t.widget;
  t 




let create ~graph ~layout =
  let (image,box) =
    MoreImage.draw_graph
      ~layout:
        (fun v -> VMap.find v layout |> MoreOption.default Gg.P2.(v 0. 0.))
      graph
  in 
  let canvas = Dom_html.(createCanvas document) in
  let widget =
    ImprovedCanvas.create
      ~canvas:canvas
      ~box
      ~size:(Gg.V2.v 210. 140.)
      ~image
  in
  { graph;
    layout;
    widget;
    arc_properties = EMap.empty;
    vertex_properties = VMap.empty
  } |> redraw



let set_layout layout t = { t with layout } |> redraw
let set_graph graph layout t =
  let (image, box) = render { t with graph } in   
  { t with graph }
  |> on_widget ImprovedCanvas.(set_box box)
  |> redraw


let get_graph t = t.graph
let get_layout t = t.layout
let get_canvas_elt t = ImprovedCanvas.canvas_of t.widget




let point_to_vertex_dist point vertex t =
  MoreGg.distance point (get_position vertex t)

let point_to_arc_dist point (src,dst) t = 
  let segment = 
    MoreGg.Segment.of_points (get_position src t) (get_position dst t)
  in
  MoreGg.Segment.distance_to ~point segment 

let arc_length (src,dst) t =
  MoreGg.distance (get_position src t) (get_position dst t)


let closest_vertex ?except position t =
  let all_vertices = VSet.elements (Graph.vertices t.graph) in
  let vertices = match except with
    | None -> all_vertices
    | Some v -> List.filter (fun u -> u <> v) all_vertices
  in 
  MoreList.find_minimum
    ~objective:(fun vertex -> point_to_vertex_dist position vertex t)
    vertices

  
let closest_arc position t =
  let arcs =
    Graph.fold_arcs ~f:MorePerv.cons t.graph []
  in
  MoreList.find_minimum
    ~objective:(fun arc -> point_to_arc_dist position arc t)
    arcs

type element =
  | Vertex of Graph.vertex
  | Arc of Graph.arc
  | Nothing

let vertex_exclusive_radius = 0.2
let min_vertex_to_vertex = 3. *. vertex_exclusive_radius

let arc_exclusive_radius (src,dst) t =
  0.15 *. MoreGg.distance (get_position src t) (get_position dst t)

let is_close_to_vertex point vertex t =
  MoreGg.distance point (get_position vertex t) < vertex_exclusive_radius

let is_on_vertex point vertex t =
  MoreGg.distance point (get_position vertex t) < 1e-6

let is_far_from_vertex point vertex t = 
  MoreGg.distance point (get_position vertex t) > min_vertex_to_vertex

let is_close_to_arc point arc t =
  point_to_arc_dist point arc t < arc_exclusive_radius arc t



let closest_element position t =
  match closest_arc position t, closest_vertex position t with
  | Some arc, Some vertex
    when is_close_to_vertex position vertex t ->
    Vertex vertex
  | Some arc, Some vertex
    when point_to_arc_dist position arc t
         < point_to_vertex_dist position vertex t ->
    Arc arc
  | _, Some vertex -> Vertex vertex
  | _ -> Nothing



let add_vertex_action position t =
  let new_vertex =
    if VSet.is_empty (Graph.vertices t.graph) then 0
    else 1 + VSet.max_elt (Graph.vertices t.graph)
  in
  ( new_vertex,
    { t with
      layout = VMap.add new_vertex position t.layout;
      graph = Graph.add_vertex new_vertex t.graph
    }
  )

let add_arc_action src dst t =
  on_graph (Graph.add_arc ~src ~dst) t


let add_vertex_and_arc source position t =
  let (new_vertex, t) = add_vertex_action position t in 
  add_arc_action source new_vertex t


let remove_arc_action ((src,dst) as arc) t =
  { t with
    graph = Graph.remove_arc ~src ~dst t.graph;
    arc_properties = EMap.remove arc t.arc_properties
  } |> redraw

let remove_vertex_action vertex t =
  let arc_properties =
    t.arc_properties
    |> ESet.fold EMap.remove (Graph.delta_in vertex t.graph)
    |> ESet.fold EMap.remove (Graph.delta_out vertex t.graph)
  in
  { t with
    graph = Graph.remove_vertex vertex t.graph;
    arc_properties;
    vertex_properties = VMap.remove vertex t.vertex_properties
  } |> redraw





let move_vertex vertex new_position t =
  { t with
    layout = VMap.add vertex new_position t.layout
  } 




let vector_of_event event t =
  let vec = ImprovedCanvas.get_vector_of_event event t.widget in
  trace (Printf.sprintf "(%f,%f)" (Gg.P2.x vec) (Gg.P2.y vec));
  vec
  

let move_canvas from_vec mouse_event t canvas =
  let new_position = vector_of_event mouse_event t in
  ( t
    |> on_widget ImprovedCanvas.(translate ~from:new_position from_vec)
    |> redraw,
    t.widget
  )


let move_canvas_thread from_vec t =
  let open Lwt in 
  ImprovedCanvas.mouse_move_up_callback
    ~on_move:(move_canvas from_vec)
    ~on_stop:(move_canvas from_vec)
    t t.widget
  >>= fun (t,widget) -> return t



let shift_out_position position vertex t =
  let v_pos = get_position vertex t in
  let candidate_position =
    Gg.V2.(
      position - v_pos
      |> unit
      |> ( * ) min_vertex_to_vertex
      |> (+) v_pos
    )
  in
  match closest_vertex ~except:vertex candidate_position t with
  | Some other when not (is_far_from_vertex candidate_position other t) ->
    None
  | otherwise -> Some candidate_position


let find_vertex_position_candidate ?except position t =
  trace "find_vertex_candidate";
  match closest_vertex ?except position t with
  | Some v when not(is_far_from_vertex position v t)
             && not(is_on_vertex position v t) ->
    shift_out_position position v t
  | Some v when is_close_to_vertex position v t -> None 
  | otherwise -> Some position



let move_vertex vertex mouse_event t canvas =
  let layout =
    let available_position =
      find_vertex_position_candidate
        ~except:vertex
        (vector_of_event mouse_event t) t
    in
    match available_position with
    | Some new_position -> VMap.add vertex new_position t.layout
    | None -> t.layout
  in
  ( { t with layout } |> redraw , t.widget)


let move_vertex_thread vertex t =
  trace "move_vertex_thread";
  let open Lwt in
  let t =
    set_vertex_moving vertex t
    |> ESet.fold set_arc_moving (Graph.delta_out vertex t.graph)
    |> ESet.fold set_arc_moving (Graph.delta_out vertex t.graph)
    |> redraw
  in 
  ImprovedCanvas.mouse_move_up_callback
    ~on_move:(move_vertex vertex)
    ~on_stop:(move_vertex vertex)
    t t.widget
  >>= fun (t, widget) ->
  t
  |> remove_vertex_moving vertex
  |> ESet.fold remove_arc_moving (Graph.delta_out vertex t.graph)
  |> ESet.fold remove_arc_moving (Graph.delta_out vertex t.graph)
  |> redraw
  |> return 
  

let move_create_arc source mouse_event t canvas =
  let position = vector_of_event mouse_event t in
  let src_position = get_position source t in
  let try_to_shift_out vertex t =
    match shift_out_position position vertex t with
    | Some dst_position -> [(src_position,dst_position)]
    | None -> []
  in
  let more_arcs =
    match closest_vertex ~except:source position t with
    | _ when not (is_far_from_vertex position source t) -> []
    | Some v when is_close_to_vertex position v t ->
      [src_position, get_position v t]
    | Some v when not (is_far_from_vertex position v t) ->
      try_to_shift_out v t
    | otherwise -> [(src_position,position)]
  in
  ( redraw ~more_arcs t ,
    t.widget
  )

let stop_create_arc source mouse_event t canvas =
  let position = vector_of_event mouse_event t in
  let try_to_shift_out vertex t =
    match shift_out_position position vertex t with
    | Some dst_position ->
      add_vertex_and_arc source dst_position t 
    | None -> t 
  in
  let t = 
    match closest_vertex ~except:source position t with
    | _ when not (is_far_from_vertex position source t) ->
      t |> redraw
    | Some v when is_close_to_vertex position v t ->
      add_arc_action source v t
    | Some v when not (is_far_from_vertex position v t) ->
       try_to_shift_out v t       
    | otherwise -> 
      add_vertex_and_arc source position t
  in
  (t |> redraw, t.widget)    
      
  

let create_arc_thread source t  =
  let open Lwt in
  ImprovedCanvas.mouse_move_up_callback
    ~on_move:(move_create_arc source)
    ~on_stop:(stop_create_arc source)
    t t.widget
  >>= fun (t, widget) ->
  return t 




let move_create_vertex mouse_event t canvas =
  let position = vector_of_event mouse_event t in
  let more_vertices =
    match find_vertex_position_candidate position t with 
    | None -> []
    | Some vec -> [vec]
  in
  ( redraw ~more_vertices t,
    t.widget
  )

let stop_create_vertex mouse_event t canvas =
  trace "stop_create_vertex";
  let position = vector_of_event mouse_event t in
  match find_vertex_position_candidate position t with
  | None -> (redraw t, t.widget)
  | Some vec ->
    let (new_vertex, t) = add_vertex_action vec t in
    (redraw t, t.widget)
   

let create_vertex_thread mouse_event t =
  trace "create_vertex_thread";
  let open Lwt in
  let (t, widget) = move_create_vertex mouse_event t t.widget in 
  ImprovedCanvas.mouse_move_up_callback
    ~on_move:move_create_vertex
    ~on_stop:stop_create_vertex
    t t.widget
  >>= fun (t,widget) ->
  return t 


let hover_thread mouse_event t =
  trace "hover_thread";
  let position = vector_of_event mouse_event t in
  let result = 
    match closest_element position t with
    | Vertex vertex when is_close_to_vertex position vertex t ->
      t
       |> set_vertex_hovered vertex
       |> redraw
       |> remove_vertex_hovered vertex
    | Arc arc when is_close_to_arc position arc t ->
      t
      |> set_arc_hovered arc
      |> redraw
      |> remove_arc_hovered arc
    | otherwise -> t |> redraw
  in
  Lwt.return result

let deletion_thread mouse_event t =
  trace "delete_thread";
  let position = vector_of_event mouse_event t in
  match closest_element position t with
  | Vertex vertex when is_close_to_vertex position vertex t ->
    remove_vertex_action vertex t |> Lwt.return 
  | Arc arc when is_close_to_arc position arc t ->
    remove_arc_action arc t |> Lwt.return 
  | otherwise -> Lwt.return t 



type event_kind =
  | Click of Dom_html.mouseEvent Js.t
  | Scroll of (Dom_html.mouseEvent Js.t * (int * int))
  | Move of Dom_html.mouseEvent Js.t


let click_thread ~on_vertex ~on_arc ~on_nothing event init t =
  trace "click_thread";
  let position =  vector_of_event event t in
  match closest_element position t with
  | Vertex vertex when is_close_to_vertex position vertex t ->
    on_vertex vertex init 
  | Arc arc when is_close_to_arc position arc t ->
    on_arc arc init
  | otherwise ->
    on_nothing init 


let create_thread ?(allow_deletion=true) ~click_handler init t =
  trace "create_thread";
  let open Lwt in
  let canvas = get_canvas_elt t in 
  pick
    [ Lwt_js_events.mousedown canvas >>= (fun ev -> return (Click ev));
      Lwt_js_events.mousewheel canvas >>= (fun ev -> return (Scroll ev));
      Lwt_js_events.mousemove canvas >>= (fun ev -> return (Move ev));
    ]
  >>= function
  | Click event when Js.to_bool (event##shiftKey) ->
    move_canvas_thread (vector_of_event event t) t
      >>= fun t -> Lwt.return (init, t)
  | Click event when Js.to_bool (event##ctrlKey) && allow_deletion ->
    deletion_thread event t >>= fun t -> Lwt.return (init,t)
  | Click event ->
    click_handler event init t
  | Scroll (event,(dx,dy)) ->
    on_widget ImprovedCanvas.(zoom ~factor:(1.15 ** float dx)) t
    |> redraw
    |> fun t -> Lwt.return (init,t)
  | Move event ->
    hover_thread event t >>= fun t -> Lwt.return (init,t)
    




let main_click_handler event () t =
  trace "main_click_handler";
  let open Lwt in
  let on_vertex vertex () =
    if event##button = 0 then create_arc_thread vertex t
    else move_vertex_thread vertex t
  in
  click_thread
    ~on_vertex
    ~on_arc:(fun arc () -> create_vertex_thread event t)
    ~on_nothing:(fun () -> create_vertex_thread event t)
    event () t
  >>= fun t -> Lwt.return ((),t)


let main_thread =
  trace "main_thread";
  create_thread
    ~allow_deletion:true
    ~click_handler:main_click_handler


let select_vertex_click_handler event default t =
  let open Lwt in
  let on_vertex vertex previous_vertex =
      t
      |> remove_vertex_selected previous_vertex
      |> set_vertex_selected vertex 
      |> redraw
      |> fun t -> Lwt.return (vertex, t)
  in
  click_thread
    ~on_vertex
    ~on_arc:(fun arc vertex -> Lwt.return (vertex, t))
    ~on_nothing:(fun vertex -> Lwt.return (vertex, t))
    event default (t |> set_vertex_selected default |> redraw)
  

let select_single_vertex_thread =
  create_thread ~click_handler:select_vertex_click_handler

let select_vertices_click_handler event vertices t =
  let open Lwt in
  let on_vertex vertex vertices =
    if VSet.mem vertex vertices then
      t
      |> remove_vertex_selected vertex
      |> redraw
      |> fun t -> Lwt.return (VSet.remove vertex vertices, t)
    else
      t
      |> set_vertex_selected vertex
      |> redraw
      |> fun t -> Lwt.return (VSet.add vertex vertices, t)
  in
  click_thread
    ~on_vertex
    ~on_arc:(fun arc vertices -> Lwt.return (vertices, t))
    ~on_nothing:(fun vertices -> Lwt.return (vertices, t))
    event vertices t 

let select_vertices_thread =
  create_thread ~click_handler:select_vertices_click_handler


let select_arcs_click_handler event arcs t =
  let open Lwt in
  let on_arc arc arcs =
    if ESet.mem arc arcs then
      t
      |> remove_arc_selected arc
      |> redraw
      |> fun t -> Lwt.return (ESet.remove arc arcs, t)
    else
      t
      |> set_arc_selected arc
      |> redraw
      |> fun t -> Lwt.return (ESet.add arc arcs, t)
  in
  click_thread
    ~on_vertex:(fun vertex arcs -> Lwt.return (arcs,t))
    ~on_arc
    ~on_nothing:(fun arcs -> Lwt.return (arcs, t))
    event arcs t

let select_arcs_thread =
  create_thread ~click_handler:select_arcs_click_handler

let select_arc_click_handler event arc t =
  let open Lwt in
  let on_arc arc previous_arc =
    Lwt.return
      (arc,
       t
       |> remove_arc_selected previous_arc
       |> set_arc_selected arc 
       |> redraw
      )
  in
  click_thread
    ~on_vertex:(fun vertex arc -> Lwt.return (arc,t))
    ~on_arc
    ~on_nothing:(fun arc -> Lwt.return (arc, t))
    event arc t

let select_single_arc_thread =
  create_thread ~click_handler:select_arc_click_handler

let rec looping_thread ~main_thread ~stop_thread init t =
  trace "looping_thread";
  let open Lwt in
  Lwt.choose
    [ main_thread init t >>= (fun (value,t) -> Lwt.return (value, t, true));
      stop_thread init t >>= (fun (value,t) -> Lwt.return (value, t, false))
    ]
  >>= fun (result, t, is_going_on) ->
  trace "looping_thread: done";
  if is_going_on then
    looping_thread ~main_thread ~stop_thread result t
  else
    Lwt.return (result,t)



let make_form_for_thread ~header ~thread ?(at_end=(fun t -> t)) init t =
  let display = Dom_html.(createDiv document) in
  let title = Dom_html.(createP document) in
  title##innerHTML <- Js.string header;
  Dom.appendChild display title;
  Dom.appendChild display ImprovedCanvas.(canvas_of t.widget);
  let (waiting_thread,wakener) = Lwt.wait () in
  let (returner,return_wakener) = Lwt.wait () in
  let open Lwt in
  Lwt.async
    (fun () ->
       let stop_thread result t =
         waiting_thread >>= fun () ->
         Lwt.wakeup return_wakener (result, t);
         Lwt.return (result,t)
       in
       looping_thread
         ~main_thread:thread
         ~stop_thread
         init t
    );
  let get_answer () =
    Lwt.wakeup wakener ();
    match Lwt.state returner with
    | Return state -> Some (at_end state)
    | otherwise -> None
  in
  Question.({ display; get_answer })


let edit_graph_form t =
  let open Question in 
  let form =
    make_form_for_thread
      ~header:"Edit the graph:"
      ~thread:main_thread
      () t
  in
  { form with
    get_answer = fun () -> MoreOption.map snd (form.get_answer ())
  }
    

let select_vertices_form t =
  let at_end (selection,t) =
    (selection, VSet.fold remove_vertex_selected selection t |> redraw)
  in         
  make_form_for_thread
    ~header:"Select vertices in the graph:"
    ~thread:select_vertices_thread
    ~at_end
    VSet.empty t


let select_arcs_form t =
  let at_end (selection, t) =
    (selection, ESet.fold remove_arc_selected selection t |> redraw)
  in
  make_form_for_thread
    ~header:"Select edges in the graph:"
    ~thread:select_arcs_thread
    ~at_end
    ESet.empty t


let select_single_vertex_form default t =
  let at_end (vertex, t) =
    (vertex, t |> remove_vertex_selected vertex |> redraw)
  in
  make_form_for_thread
    ~header:"Choose one vertex:"
    ~thread:select_single_vertex_thread
    ~at_end
    default t


let select_single_arc_form default t =
  let at_end (arc, t) =
      (arc, t |> remove_arc_selected arc |> redraw)
  in
  make_form_for_thread
    ~header:"Choose one arc:"
    ~thread:select_single_arc_thread
    ~at_end
    default
    t
