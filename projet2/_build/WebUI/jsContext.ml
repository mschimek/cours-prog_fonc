(*** CONTEXT ***)

type node =
  {
    container : Dom_html.divElement Js.t;
    content : Dom_html.divElement Js.t;
    hide_button : Dom_html.buttonElement Js.t;
    mutable is_hidden : bool;
    mutable title : Dom_html.paragraphElement Js.t option;
  }

    
type context =
  {
    dom_node : node Tree.zipped_tree;
    document : Dom_html.document Js.t;
    switch_on : bool;
  }



(*** MANIPULATION OF CONTEXT ***)
  
open State
open State.Infix

let get_zipped_tree = get >>= fun context -> return context.dom_node
let get_document = get >>= fun context -> return context.document


let lift_doc creater =
  get_document >>= fun document -> return (creater document)

let apply_on_zipped_tree fct =
  get >>= fun context -> set { context with dom_node = fct context.dom_node }

let get_current_node =
  get_zipped_tree >>= fun zipper ->
  match Tree.zip_current_node zipper with
  | None -> assert false
  | Some node -> return node 

let switch_hide_property node =
  if node.is_hidden then
    begin
      Dom.appendChild node.container node.content;
      node.hide_button##innerHTML <- Js.string "hide"
    end
  else
    begin
      Dom.removeChild node.container node.content;
      node.hide_button##innerHTML <- Js.string "show"
    end;
  node.is_hidden <- not node.is_hidden
    

let create_container_title text =
  lift_doc Dom_html.createSpan >>= fun p_elt -> 
  p_elt##innerHTML <- Js.string text;
  p_elt##className <- Js.string "jsContainerTitle";
  return p_elt

let rec handle_button_click node button =
  let open Lwt in 
  Lwt_js_events.click button >>= fun mouse_event -> 
  switch_hide_property node;
  handle_button_click node button 

let start_hide_show_event_handling node button =
  let open Lwt in 
  Lwt_js_events.async
    (fun () ->
       handle_button_click node button
    )

  

let create_hide_button = 
  let _type = Js.string "button" in
  lift_doc (Dom_html.createButton ~_type) >>= fun button ->
  button##innerHTML <- Js.string "hide";
  button##className <- Js.string "hideShowButton";
  return button 

let create_container_node =
  lift_doc Dom_html.createDiv >>= fun container ->
  lift_doc Dom_html.createDiv >>= fun content ->
  create_hide_button >>= fun hide_button ->
  container##className <- Js.string "jsInterContainer";
  content##className <- Js.string "jsInterContent";
  Dom.appendChild container hide_button;
  Dom.appendChild container content;
  let node = 
    { container;
      content;
      hide_button;
      is_hidden = false;
      title = None
    }
  in
  start_hide_show_event_handling node hide_button;
  return node 
 

let set_title title =
  get_current_node >>= fun node ->
  match node.title with
  | Some p_elt ->
    p_elt##innerHTML <- Js.string title;
    return ()
  | None ->
    create_container_title title >>= fun p_elt -> 
    Dom.insertBefore node.container p_elt (node.container##firstChild);
    node.title <- Some p_elt;
    return ()
    

let elt_scroll_down elt =
  elt##scrollTop <- elt##scrollHeight
  


let create_new_node_with_father jsElt =
  State.get >>= fun context ->
  create_container_node >>= fun node ->
  Dom.appendChild jsElt node.container;
  return node
    
let create_new_node ?title =
  get_current_node >>= fun node -> 
  create_new_node_with_father node.content >>= fun new_node ->
  apply_on_zipped_tree
    (fun node -> let open Tree in
      node 
      |> insert_subtree ~position:Leftmost (singleton new_node)
      |> zip_down
      |> zip_to_left
    ) >>
  match title with
  | None -> return ()
  | Some title -> set_title title
  
let append_to_current_node dom_element =
  get_current_node >>= fun node ->
  Dom.appendChild node.content dom_element;
  elt_scroll_down (Dom_html.document##body);
  return ()




let initialize =
  get_document >>= fun document -> 
  create_new_node_with_father (document##body) >>= fun new_node ->
  set { dom_node = Tree.zip (Tree.NonEmpty (Tree.singleton new_node));
        document;
        switch_on = true;
      }


let init_context () =
  initialize
    { dom_node = Tree.zip Tree.Empty;
      document = Dom_html.document;
      switch_on = true;
    }
  |> snd



(*** EFFECTS ***)

type effect = context -> (unit * context)


let no_effect context = ((),context)
let apply (effect:effect) context = effect context |> snd
let compose = State.Infix.(>>)


let increment = create_new_node ?title:None

let get_up context =
  { context with dom_node = context.dom_node |> Tree.zip_up }

let decrement = update_state get_up

let end_line =
  get_document >>= fun document ->
  let endline = Dom_html.createBr document in
  append_to_current_node endline

let msg string =
  get_document >>= fun document ->
  let paragraph = Dom_html.createSpan document in
  paragraph ## innerHTML <- Js.string string;
  append_to_current_node paragraph


let switch_on =
  State.update_state (fun context -> { context with switch_on = true })
let switch_off =
  State.update_state (fun context -> { context with switch_on = false })
    


(*** PICTURES ***)

module AddDrawable = functor (Value : MoreModules.DrawableType) ->
struct
         

  let show ?(width=90) ?(height=60) value =
    let (image, box) = Value.to_image value in
    State.get >>= fun context ->
    let canvas = Dom_html.createCanvas context.document in
    let widget =
      ImageWidget.create
        ~canvas
        ~box
        ~size:(Gg.V2.v (float width) (float height))
        ~image:image 
    in
    canvas##style##border <- Js.string "black solid 1px";
    ImageWidget.start_event_handlers widget;
    append_to_current_node canvas
end 

