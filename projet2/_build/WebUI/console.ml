module Make =
  functor (Context : BasicTypes.ObservationContext) ->
  functor (Emulator : BasicTypes.Emulator
           with type context = Context.context
          ) ->
  functor (Value : MoreModules.DrawableType) ->
struct

  type value = Value.t
                 
  module SMap = Map.Make(String)
  
  type nodes =
    { environment : Dom_html.divElement Js.t;
      input : Dom_html.textAreaElement Js.t;
      submit : Dom_html.buttonElement Js.t;
      messages : Dom_html.divElement Js.t;
      step : Dom_html.buttonElement Js.t;
      run : Dom_html.buttonElement Js.t;
      run_all : Dom_html.buttonElement Js.t
    }
  
  type computation =
      Comp :
        'result Emulator.t * ('result -> unit t) -> computation
  and run =
    Run : 'result Emulator.run * ('result -> unit t) -> run
  
  and context =
    { dictionary : (Value.t * Dom_html.element Js.t) SMap.t;
      nodes : nodes;
      obs_context : Emulator.context;
      current_run : run option;
      computations : computation MoreQueue.t;
      current_directive : (unit -> unit t Lwt.t) option;
    }

  and 'any t = ('any,context) State.m

  
  open State.Infix (* import >>= *)
  
  let get_environment_div =
    State.get >>= fun context -> State.return context.nodes.environment

  let get_input_area =
    State.get >>= fun context -> State.return context.nodes.input

  let get_submit_button =
    State.get >>= fun context -> State.return context.nodes.submit

  let get_messages_div =
    State.get >>= fun context -> State.return context.nodes.messages

  
  let create_control () =
    let open Dom_html in
    let control_div = createDiv document in
    control_div##id <- Js.string "control_div";
    Dom.appendChild (document##body) control_div;
    control_div

  let set_style_nodes nodes =
    nodes.input##className <- Js.string "input_area";
    nodes.input##value <- Js.string "Master, give me your command here!";
    nodes.submit##innerHTML <- Js.string "Interprete!";
    nodes.environment##id <- Js.string "env";
    nodes.step##innerHTML <- Js.string "Step";
    nodes.run##innerHTML <- Js.string "Run";
    nodes.run_all##innerHTML <- Js.string "Run all"


  let create_nodes control_div =
    let open Dom_html in
    let nodes =
      { input = createTextarea document;
        submit = createButton ~_type:(Js.string "submit") document;
        messages = createDiv document;
        environment = createDiv document;
        step = createButton document;
        run = createButton document;
        run_all = createButton document;
      }
    in
    set_style_nodes nodes;
    Dom.appendChild control_div (nodes.messages);
    Dom.appendChild control_div nodes.input;
    Dom.appendChild control_div nodes.submit;
    Dom.appendChild control_div (createBr document);
    Dom.appendChild control_div nodes.step;
    Dom.appendChild control_div nodes.run;
    Dom.appendChild control_div nodes.run_all;
    Dom.appendChild (document##body) nodes.environment;
    nodes



  (* *** Messages *** *)
  
  let empty_messages =
    get_messages_div >>= fun msg_div ->
    Messages.empty_messages msg_div;
    State.return ()
    
  let send_messages message =
    get_messages_div >>= fun msg_div ->
    Messages.update_messages msg_div message;
    State.return ()
       

  (* *** dictionary *** *)


  let get_dictionary =
    State.get >>= fun context -> State.return context.dictionary

  let set_dictionary dictionary =
    State.update_state (fun ctxt -> { ctxt with dictionary })
  
      


  let remove_node node =
    get_environment_div >>= fun env_div ->    
    Dom.removeChild env_div node;
    State.return ()

  let prepare_canvas value =
    let canvas = Dom_html.createCanvas Dom_html.document in
    let (image,box) = Value.to_image value in
    let (width, height) = (45., 30.) in
    let size = Gg.V2.v width height in
    let widget =
      ImageWidget.create ~canvas ~image ~box ~size
    in
    ImageWidget.start_event_handlers widget;
    (canvas##style)##border <- Js.string "black solid 1px";
    canvas

  
  let scroll_down_environment =
    get_environment_div >>= fun env_div ->
    env_div##scrollTop <- env_div##scrollHeight;
    State.return ()
      
  
  let add_node key value =
    let open Dom_html in
    get_environment_div >>= fun env_div ->
    let node = createDiv document in
    let span = createSpan document in
    let canvas = prepare_canvas value in 
    node##className <- Js.string "dict_node";
    span##innerHTML <- Js.string (Printf.sprintf "%s: " key);
    Dom.appendChild node span;
    Dom.appendChild node canvas;
    Dom.appendChild env_div node;
    scroll_down_environment >> 
    State.return node 



  (* *** dictionary *** *)
      
  let remove key =
    get_dictionary >>= fun dict ->
    if SMap.mem key dict then
      begin
        let (_value,node) = SMap.find key dict in 
        remove_node node >> 
        set_dictionary (SMap.remove key dict)
      end
    else State.return ()
        

  
  let add key value =
    get_dictionary >>= fun dict ->
    remove key >>
    add_node key value >>= fun node ->
    set_dictionary (SMap.add key (value,node) dict)

  let find key =
    get_dictionary >>= fun dict ->
    if SMap.mem key dict then
      let (value,_node)= SMap.find key dict in 
      State.return (Some value)
    else State.return None
  
  let rename_value old_key new_key =
    find old_key >>= function
    | None -> State.return ()
    | Some value ->
      remove old_key >>
      add new_key value
      


  (* *** input area *** *)

  let get_input =
    get_input_area >>= fun textarea ->
    State.return (Js.to_string (textarea##value))
      
  let set_input text =
    get_input_area >>= fun textarea ->
    textarea##value <- Js.string text;
    State.return ()



  (* *** action on observation context *** *)
    

  let act effect =
    State.get >>= fun context ->
    State.set
      { context with
        obs_context = Context.apply effect context.obs_context
      } >>
    State.return ()




  (* *** questions *** *)

  let get_directive =
    let open State.Infix in
    State.get >>= fun context ->
    State.return context.current_directive
  
  let set_directive current_directive =
    let open State.Infix in
    State.update_state
      (fun context -> { context with current_directive })
                         

  let add_directive thread_option =
    let open State.Infix in
    State.get >>= fun context ->
    set_directive thread_option
    

  
  let ask question continuation =
    let open Lwt in
    let directive () =
      let overlay_div = Dom_html.createDiv Dom_html.document in
      overlay_div##className <- Js.string "overlay_div";
      let center_div = Dom_html.createDiv Dom_html.document in
      center_div##className <- Js.string "question_div";
      Dom.appendChild overlay_div center_div;
      Dom.appendChild (Dom_html.document##body) overlay_div;
      Question.propose ~father:center_div question >>= fun result ->
      Dom.removeChild (Dom_html.document##body) overlay_div;
      return (continuation result)
    in
    add_directive (Some directive)
      

  

  (* *** computation *** *)



  let get_obs_context =
    State.get >>= fun context -> State.return context.obs_context

  let set_obs_context obs_context =
    State.update_state (fun ctxt -> { ctxt with obs_context })


  let set_step_button_state =
    State.get >>= fun context ->
    let button = context.nodes.step in 
    button##disabled <-
      Js.bool (context.current_run = None
               && MoreQueue.is_empty context.computations);
    State.return ()
    
      
  let get_run =
    State.get >>= fun context -> State.return context.current_run

  let set_run current_run =
    State.get >>= fun context ->
    let button = context.nodes.run in
    button##disabled <- Js.bool (current_run = None);
    State.update_state (fun ctxt -> { ctxt with current_run }) >>
    set_step_button_state 

  
  let get_computations =
    State.get >>= fun context -> State.return context.computations

  let set_computations computations =
    State.get >>= fun context ->
    let button = context.nodes.run_all in
    button##disabled <- Js.bool (MoreQueue.is_empty computations);
    State.set { context with computations } >>
    set_step_button_state 


  
  let register computation continuation =
    get_computations >>= fun computations ->
    let new_computations =
      MoreQueue.add_right computations (Comp (computation,continuation)) 
    in
    set_computations new_computations

  
  let load_next_run =
    get_computations >>= fun computations ->
    match MoreQueue.observe_left computations with
    | Some (Comp (comp,cont),next_computations) ->
      send_messages (Tree.Node ("Loading next computation",[])) >>
      set_run (Some (Run (Emulator.init_run comp, cont))) >>
      set_computations next_computations
    | None ->
      send_messages (Tree.Node ("Work completed",[])) >>
      State.return ()



  let rec one_step (Run (run,continuation)) =
    get_obs_context >>= fun obs_context ->
    let open Emulator in
    let (eval_res,obs_context) = Emulator.step run obs_context in
    set_obs_context obs_context >>
    match eval_res with
    | Done result ->
      set_run None >>
      continuation result 
    | More next_run ->
      set_run (Some (Run (next_run,continuation)))
    | Free next_run ->
      one_step (Run (next_run, continuation))

  let execute_next_step =
    get_run >>= function
    | None -> load_next_run
    | Some run -> one_step run 

  let execute_current_run =
    let rec loop () =
      get_run >>= function
      | None -> State.return ()
      | Some run ->
        one_step run >>
        loop ()
    in loop ()


  let execute_all =
    let rec loop () =
      execute_current_run >>
      get_computations >>= fun computations ->
      if MoreQueue.is_empty computations then State.return ()
      else 
        begin
          load_next_run >>
          loop ()
        end
    in loop ()

  (* *** events *** *)


  type callbacks =
    { mutable on_submit : unit t;
      mutable on_step : unit t;
      mutable on_run : unit t;
      mutable on_run_all : unit t
    }

  let default_on_submit =
    get_input >>= fun text ->
    send_messages Tree.( Node ("Read:", [Node (text,[])]))

  let default_on_step = execute_next_step
  let default_on_run = execute_current_run
  let default_on_run_all = execute_all

  let callbacks =
    { on_submit = default_on_submit;
      on_step = default_on_step;
      on_run = default_on_run;
      on_run_all = default_on_run_all
    }

  let on_submit_click on_submit =
    callbacks.on_submit <- on_submit 

  let on_step_click on_step =
    callbacks.on_step <- on_step

  let on_run_click on_run =
    callbacks.on_submit <- on_run

  let on_run_all_click on_run_all =
    callbacks.on_run_all <- on_run_all




  (* *** initialization *** *)
  
  let initial_context obs_context =
    { dictionary = SMap.empty;
      nodes = create_nodes (create_control ());
      obs_context = obs_context;
      current_run = None;
      computations = MoreQueue.empty;
      current_directive = None
    }


  let set_click_handler current_context target callback =
    let open Lwt in 
    Lwt_js_events.click target 
    >>= (fun mouse_ev ->
        current_context
        |> callback
        |> snd
        |> Lwt.return 
      )

  let rec handler current_context =
    let open Lwt in 
    match current_context.current_directive with 
    | None ->
      Lwt.pick 
        [ set_click_handler current_context
            current_context.nodes.submit callbacks.on_submit;
          set_click_handler current_context
            current_context.nodes.step callbacks.on_step;
          set_click_handler current_context
            current_context.nodes.run callbacks.on_run;
          set_click_handler current_context
            current_context.nodes.run_all callbacks.on_run_all;
        ]
      >>= handler
    | Some thread ->
      thread () >>= fun state_action ->
      let ((),new_context) =
        current_context
        |> ( set_directive None
             >> state_action )
      in 
      handler new_context

  
  let start_event_loop ?(before=State.return ()) obs_context =
    let open Lwt in 
    let initialize =
      set_run None >>
      set_computations MoreQueue.empty >>
      before
    in
    let ((),init_context) = initialize (initial_context obs_context) in
    Lwt.async (fun () -> handler init_context)




end
