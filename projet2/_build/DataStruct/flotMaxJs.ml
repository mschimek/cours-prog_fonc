
module GraphValue =
struct
  type t = Graph.t  * (int*int) Graph.EMap.t * int * Graph.vertex 
	     *Graph.vertex * (Graph.vertex -> Gg.p2)
								      
  let to_image (graph,emap,flow,source,sink,layout) =
    MoreImage.draw_graph_flow ~layout graph emap 
end
  
module DrawSolution =
  JsContext.AddDrawable(GraphValue)

module Console =
  Console.Make
    (JsContext)
    (JsEmulation.Emulator)
    (GraphValue)

let string_node txt =
  let span = Dom_html.(createSpan document) in
  span##innerHTML <- Js.string txt;
  span

let register_algo (graph,edges,layout,source,sink) = 
  if Graph.VSet.is_empty (Graph.vertices graph) then
    Console.send_messages
      Tree.(Node ("Empty graph", [] ))
  else
    let computation  = FlotMax_Computation.edmonds_karp_init 
			~graph 
			~edges
			~layout
			~source
			~sink
    in
    let continutation (graph, edges, flow, source, sink) 
      = Console.act(DrawSolution.show
           ~width:120 ~height:90
           (graph,edges,flow,source,sink,layout))
         
   in 
    Console.register computation continutation
   
let to_list eset =
  Graph.ESet.fold 
    (fun elem list -> elem::list)
    eset
    []

let flatten list =
  List.fold_left 
    (fun  list1 eset -> (to_list eset)::list1)
    []
    list
 |> List.flatten
 
     

let edges graph = 
  let vertices = Graph.vertices graph in
  Graph.VSet.fold
    (fun vertex list -> (Graph.delta_out vertex graph)::list)  
    vertices
    []
  |> flatten
  |> List.fold_left 
       (fun emap elem -> Graph.EMap.add elem (0,0) emap)
       Graph.EMap.empty
 
let question_arc_capacity src dest =
  let open Question in
  let open Question.Infix in
  ask_int (Printf.sprintf "capacity of this arc : (%d,%d)" src dest)
  >>= fun weight -> Question.return ((src,dest), weight)


let question_capacities editable_graph =
  let graph = EditableGraph.get_graph editable_graph in
  let edges = Graph.fold_arcs
		~f:(fun arc list -> arc::list) 
		graph
		[]
  in 			
  let list_of_questions =
    List.map 
      (fun (src,dest) -> question_arc_capacity src dest)
      edges
  in 
  Question.of_list list_of_questions


let question_new_graph =
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


let question_capacitated_graph =
  let open Question.Infix in 
  question_new_graph >>= fun editable_graph -> 
  question_capacities editable_graph >>= fun list_of_capacities ->
  Question.return 
    ( EditableGraph.get_graph editable_graph, 
      EditableGraph.get_layout editable_graph,
      list_of_capacities)

let create_emap capacities =
  List.fold_left 
    (fun map ((src,dest),cap) -> Graph.EMap.add (src,dest) (0,cap) map)
    Graph.EMap.empty
    capacities
    

let action_define_capacitated_graph name =
  let action_on_graph (graph, layout, capacities) =
    Console.add name (
		  graph,
		  (create_emap capacities),
		  0,
		  0,
		  0,
		  (fun vertex ->
		   Graph.VMap.unsafe_find vertex layout)
		)
  in
  Console.ask
    question_capacitated_graph action_on_graph

let do_algo_from_name name =
  let open State.Infix in
  Console.find name >>= function
  | None ->
    Console.send_messages
      Tree.(Node ("Undefined graph.",[]))
  | Some (graph,emap,flow,source,sink,layout) ->
    register_algo (graph,emap,layout,source,sink)

 
let command_algo =
  let open Control.Line in
  Control.Argument.ident
    "graph"
    "the graph on which edmonds-karp shall be done."
  @* do_algo_from_name

let command_define =
  let open Control.Line in
  Control.Argument.ident
    "graph_name"
    "A name for the graph that will be defined"
  @* action_define_capacitated_graph 







let all_commands =
  let open Control.Command in 
  empty
  ++ register
    ~doc:"Perform a calculation of a maximal flow"
    "algo"
    command_algo
  ++ register
       ~doc:"define a new graph"
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
