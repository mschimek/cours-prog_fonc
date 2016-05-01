module GraphValue =
struct
  type t = Graph.t * (Graph.vertex -> Gg.p2)
  let to_image (graph,layout) =
    MoreImage.draw_graph ~layout graph
end

module GraphAndEdgeSet =
struct
  type t =
    Graph.t * (Graph.vertex -> Gg.p2) * Graph.ESet.t
                                                 
  let to_image (graph,layout,eset) =
    let arc_properties arc =
      if Graph.ESet.mem arc eset then
        [MoreImage.Properties.drawing_color Gg.Color.red]
      else
        []
    in
    MoreImage.draw_graph
      ~arc_properties
      ~layout
      graph
end

module Console =
  Console.Make
    (JsContext)
    (JsEmulation.Emulator)
    (GraphValue)


module DrawSolution =
  JsContext.AddDrawable(GraphAndEdgeSet)





let string_node txt =
  let span = Dom_html.(createSpan document) in
  span##innerHTML <- Js.string txt;
  span


let register_bfs (graph,layout) =
  if Graph.VSet.is_empty (Graph.vertices graph) then
    Console.send_messages
      Tree.(Node ("Empty graphQQQQQQQ",[]))
  else
    let default_source =
      Graph.VSet.choose (Graph.vertices graph)
    in    
    let computation = Bfs.bfs ~graph ~layout in
    let action_on_edge_set edges =
      Console.act
        (DrawSolution.show
           ~width:120 ~height:90
           (graph,layout,edges)
        )
    in
    let map_layout =
      Graph.fold_vertices
        ~f:(fun vertex ->
            Graph.VMap.add vertex (layout vertex)
          )
        graph
        Graph.VMap.empty
    in
    let question =
      let editable_graph =
        EditableGraph.create
          ~graph
          ~layout:map_layout
      in 
      let form =
        EditableGraph.select_single_vertex_form
          default_source
          editable_graph
      in
      Question.question_of_form
        ~title:(string_node "Choose a source for the bfs")
        ~form
      |> Question.map ~f:(fun (vertex,edit) -> vertex)
    in
    Console.ask question
      (fun source ->
         Console.register
           (computation source)
           action_on_edge_set
      )


let define_graph name =
  let question =
    let init_graph =
      EditableGraph.create
        ~graph:Graph.empty
        ~layout:Graph.VMap.empty
    in
    let form =
      EditableGraph.edit_graph_form init_graph
    in
    Question.question_of_form
      ~title:(string_node "Edit this graph.")
      ~form
  in
  let action_on_graph editable_graph =
    let graph = EditableGraph.get_graph editable_graph in
    let layout = EditableGraph.get_layout editable_graph in
    Console.add name
      (graph,
       (fun vertex ->
          Graph.VMap.unsafe_find vertex layout)
      )
        
  in    
  Console.ask question action_on_graph 


let do_bfs_from_name name =
  let open State.Infix in
  Console.find name >>= function
  | None ->
    Console.send_messages
      Tree.(Node ("Undefined graph.",[]))
  | Some (graph,layout) ->
    register_bfs (graph,layout)

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
let ask_command =
  let open Control.Line in
  return ask_graph_action


let command_myCommand = 
 let open Control.Line in
  Control.Argument.ident
    "graph"
    "myComand : the graph on which bfs shall be done."
  @* do_bfs_from_name 


let command_bfs =
  let open Control.Line in
  Control.Argument.ident
    "graph"
    "the graph on which bfs shall be done."
  @* do_bfs_from_name

let command_define =
  let open Control.Line in
  Control.Argument.ident
    "graph_name"
    "A name for the graph that will be defined"
  @* define_graph



let all_commands =
  let open Control.Command in 
  empty
  ++ register
       ~doc:"Perform a breadth first search"
       "bfs"
       command_bfs
  ++ register
       ~doc:"Define a new graph"
       "define"
       command_define
  ++ register
       ~doc:"MyComand"
       "my"
       command_myCommand
  ++ register
       ~doc:"ask for a graph"
       "ask"
       ask_command
       


let () =
  Console.on_submit_click
    begin
      let open State.Infix in
      Console.get_input >>= fun string ->
      let stream = MoreStream.from_string string in
      let { Control.Command.printed;
            Control.Command.computed } =
        Control.Command.execute
          all_commands
          stream 
      in
      Console.send_messages printed >>
      match computed with
      | Some computation -> computation
      | None -> State.return ()
    end


let () =
  Console.start_event_loop
    (JsContext.init_context ())
