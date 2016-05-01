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
