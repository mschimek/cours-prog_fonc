module Int = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end

module Str = struct
  type t = string
  let compare = String.compare
  let to_string str = str
end

let () = Random.self_init ()

module IHeap = JsHeap.Make(Int)(Str)


module Console = Console.Make(JsContext)(JsEmulation.Emulator)(IHeap)


let send_undefined heap_name = 
  let msg = Printf.sprintf "Heap {%s} is undefined" heap_name in
  Console.send_messages Tree.(Node (msg,[]))

let if_defined name kont =
  let open State.Infix in 
  Console.find name >>= function
  | None -> send_undefined name
  | Some heap -> kont heap





let rename_action new_name old_name () =
  let open State.Infix in 
  let action = 
    if_defined old_name @@ fun heap -> 
    Console.add new_name heap >>
    if old_name <> "last" then Console.remove old_name
    else State.return ()
  in
  action 

    
let copy_action new_name old_name () =
  let open State.Infix in
  let action =
    if_defined old_name @@ fun old_heap ->
    Console.add new_name old_heap
  in
  action


let remove_action heap_name () =
  let open State.Infix in
  let action = 
    if_defined heap_name @@ fun _heap ->
    Console.remove heap_name
  in
  action 



let insert_action ?(heap_name="last") values () =
  let insert_one heap_comp int =
    let open JsEmulation.Computation.Infix in
    heap_comp >>= IHeap.insert int ""
  in
  let open State.Infix in
  if_defined heap_name @@ fun heap ->
  let heap_comp = JsEmulation.Computation.return heap in
  let computation = List.fold_left insert_one heap_comp values in
  let continuation = Console.add heap_name in
  Console.register computation continuation



let remove_min_action heap_name () =
  if_defined heap_name @@ fun heap ->
  Console.register
    (IHeap.remove_minimum heap)
    (fun result -> Console.add heap_name result)
     


let merge_action left_heap right_heap result_name () =
  if_defined left_heap @@ fun left ->
  if_defined right_heap @@ fun right ->
  let computation = IHeap.merge left right in
  let continuation = Console.add result_name in
  Console.register computation continuation


let graph =
  Graph.(
    empty
    |> add_arc ~src:1 ~dst:3
    |> add_arc ~src:1 ~dst:2
  )

let layout =
  List.fold_right
    (fun (v,x,y) -> Graph.VMap.add v (Gg.P2.v x y))
    [ (1,0.0,0.0);
      (2,2.0,0.0);
      (3, 1.0,2.0);
    ]
    Graph.VMap.empty



let ask_graph_action () =
  let graph_widget =
    EditableGraph.create ~graph ~layout
  in
  let title = Dom_html.(createSpan document) in
  title##innerHTML <- Js.string "Draw a graph";
  Console.ask
    begin
      let open Question.Infix in
      Question.pack ~title:Dom_html.(createSpan document)
        begin
          Question.question_of_form
            ~title
            ~form:(EditableGraph.edit_graph_form graph_widget)
          >>= fun widget -> 
          title##innerHTML <- Js.string "Select vertices";
          Question.question_of_form
            ~title
            ~form:(EditableGraph.select_vertices_form widget)
          >>= fun (vertices,widget) ->
          title##innerHTML <- Js.string "Select arcs";
          Question.question_of_form
            ~title
            ~form:(EditableGraph.select_arcs_form widget)
          >>= fun (arcs,widget) ->
          title##innerHTML <- Js.string "Select vertex";
          Question.question_of_form
            ~title
            ~form:(EditableGraph.select_single_vertex_form (-1) widget )
          >>= fun (vertex,widget) ->
          title##innerHTML <- Js.string "Select arc";
          Question.question_of_form
            ~title
            ~form:(EditableGraph.select_single_arc_form (-1,-1) widget)
          |> Question.map ~f:(fun (arc,widget) -> (vertices,arcs,widget))
        end        
    end
    (fun (selected_vertices,selected_arcs,widget) ->
       let graph = EditableGraph.get_graph widget in
       let layout = EditableGraph.get_layout widget in
       let x vertex =
         Graph.VMap.find vertex layout
         |> MoreOption.map Gg.P2.x
         |> MoreOption.default 42.
       in
       let y vertex =
         Graph.VMap.find vertex layout
         |> MoreOption.map Gg.P2.y 
         |> MoreOption.default 42. 
       in
       let print_vertex vertex =
         Printf.sprintf "%d, at (%f,%f)" vertex (x vertex) (y vertex)
       in
       let print_arc (src,dst) =
         Printf.sprintf "%d -> %d" src dst
       in
       let vertices =
         Graph.fold_vertices ~f:MorePerv.cons graph [] 
         |> List.map print_vertex
         |> List.map Tree.singleton
       in
       let arcs =
         Graph.fold_arcs ~f:MorePerv.cons graph []
         |> List.map print_arc
         |> List.map Tree.singleton
       in
       let selected_vertices_descr =
         Graph.VSet.elements selected_vertices
         |> List.map print_vertex
         |> List.map Tree.singleton
       in
       let selected_arcs_descr =
         Graph.ESet.elements selected_arcs
         |> List.map print_arc
         |> List.map Tree.singleton
       in
       Console.send_messages
         Tree.(
           Node (
             "graph received",
             [ Node ("Vertices:", vertices);
               Node ("Arcs:", arcs);
               Node ("Vertices selected:", selected_vertices_descr);
               Node ("Arcs selected:", selected_arcs_descr)                 
             ]
           )
         )
    )


let ask_action () =
  let title = Dom_html.(createP document) in
  title##innerHTML <- Js.string "Please answer the following questions.";
  Console.ask
    ( Question.pack
        ~title
        (let open Question.Infix in 
         begin
           (fun i f s -> Printf.sprintf "You gave me (%d,%f,%s)" i f s)
           *> Question.ask_int "an int"
           +> Question.ask_float "a float"
           +> Question.ask_string "a string"
         end >>= fun s -> Question.ask_int s 
        )
    )
    (fun int ->
       let text =
         Printf.sprintf "You gave me a %d" int
       in
       Console.send_messages (Tree.Node (text,[]))
    )


let ask_command =
  let open Control.Line in
  return ask_graph_action





let int_or_int_list_argument name =
  let open Control.Argument in
  either name
    (list_of "keys" (int "key" "keys to be inserted"))
    (map ~f:(fun key -> [key]) (int "key" "a key to be inserted"))


let insert_command = 
  let open Control.Line in
  ( int_or_int_list_argument "keys"
    @+ "in"
    @^ Control.Argument.ident "heap" "the name of the heap in which to insert"
    @* fun heap_name ints -> insert_action ~heap_name ints
  )
  <+> (
    int_or_int_list_argument "key"
    @* insert_action ~heap_name:"last"
  )


let remove_min_command =
  let open Control.Line in
  Control.Argument.ident ~default:"last" "heap"
    "the name of the heap from which minimum must be removed"
  @* remove_min_action


let merge_command =
  let open Control.Line in
  Control.Argument.ident ~default:"last" "heap1" "name of first heap to merge"
  @+ "with"
  @^ Control.Argument.ident "heap2" "name of second heap to merge"
  @+
  begin
    ( "as"
      @^ Control.Argument.ident "name" "name for the merged heap"
      @* (fun name heap2 heap1 -> merge_action heap1 heap2 name)
    )
    <+> ( Control.Line.return
            (fun heap2 heap1 -> merge_action heap1 heap2 "last")
        )
  end

let rename_command =
  let open Control.Line in
  Control.Argument.ident "old_name" "name of heap to be renamed"
  @+ Control.Argument.ident "new_name" "new name for that heap"
  @* rename_action


let copy_command =
  let open Control.Line in
  Control.Argument.ident "original_name" "name of heap to be copied"
  @+ Control.Argument.ident "new_name" "new name for the copy"
  @* copy_action

let remove_command =
  let open Control.Line in
  Control.Argument.ident "name" "name of heap to be removed"
  @* remove_action




let command =
  let open Control.Command in
  empty
  ++ register ~doc:"insert an integer" "insert" insert_command
  ++ register ~doc:"remove the minimum" "remove_min" remove_min_command
  ++ register ~doc:"merge two heaps" "merge" merge_command
  ++ register ~doc:"rename a heap" "rename" rename_command
  ++ register ~doc:"copy a heap" "copy" copy_command
  ++ register ~doc:"remove a heap" "remove" remove_command
  ++ register ~doc:"testing forms" "ask" ask_command





let () =
  Console.on_submit_click
    begin
      let open State.Infix in
      let open Control.Command in
      Console.get_input >>= fun input_text ->
      let input_stream = MoreStream.from_string input_text in
      let comp = Control.Command.execute command input_stream in
      Console.send_messages comp.printed >>
      match comp.computed with
      | Some action -> action ()
      | None -> State.return ()
    end
   

let () =
  Console.start_event_loop
    ~before:(
      let open State.Infix in
      Console.add "empty" IHeap.empty >>
      Console.add "last" IHeap.empty
    )
    (JsContext.init_context ())

