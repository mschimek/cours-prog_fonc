
module GraphValue =
struct
  type t = Graph.t  * (int*int) Graph.EMap.t * int * Graph.vertex 
	     *Graph.vertex * (Graph.vertex -> Gg.p2)
				
  let vertex_properties ~source ~sink vertex =
    let open MoreImage in
    if vertex = source then 
      Properties.([filling_color Gg.Color.red])
    else if vertex = sink then
      Properties.([filling_color Gg.Color.green])
    else
      Properties.([filling_color Gg.Color.black])
  let to_image (graph,emap,flow,source,sink,layout) =
    
    MoreImage.draw_graph_flow 
      ~vertex_properties:(vertex_properties ~source ~sink) 
      ~layout 
      graph 
      emap 
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

let question_source_sink editableGraph =
  let open Question in
  let open Question.Infix in
  let title = Dom_html.(createSpan document) in
  (*title##innerHTML <- Js.string "Select source";*)
  Question.question_of_form
    ~title
    ~form:(EditableGraph.select_single_vertex_form (-1) editableGraph)
  >>= fun source ->
  (*title##innerHTML <- Js.string "Select sink"; *)
  Question.question_of_form
    ~title
    ~form:(EditableGraph.select_single_vertex_form (-1) editableGraph)
  >>= fun sink ->
  return (fst source, fst sink) 
	 
 
let _ = question_source_sink 

let question_arc_capacity src dest =
  let open Question in
  let open Question.Infix in
  ask_int (Printf.sprintf "capacity of this arc : (%d,%d)" src dest)
  >>= fun weight -> Question.return ((src,dest), weight)

let question_set_chosen_arc_capacity src editableGraph =
  let open Question.Infix in
  let title = Dom_html.(createSpan document) in
  Question.question_of_form
    ~title
    ~form:(EditableGraph.select_single_arc_form (-1,-1) editableGraph)
  >>= fun (source,editableGraph) ->
  question_arc_capacity (fst source) (snd source)
    

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

(* il faut ajouter cet argument inutile pour que on puisse definer plusieurs
 graphes, il y a peut-etre une meilleure solution mais je ne l'a pas trouve *)
let question_new_graph name =
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

let question_capacitated_graph name =
  let open Question.Infix in 
  (question_new_graph name) >>= fun editable_graph -> 
  question_capacities editable_graph >>= fun list_of_capacities ->
  question_source_sink editable_graph >>= fun (source,sink) ->
  Question.return 
    ( EditableGraph.get_graph editable_graph, 
      EditableGraph.get_layout editable_graph,
      source,
      sink,
      list_of_capacities)

let create_emap capacities =
  List.fold_left 
    (fun map ((src,dest),cap) -> Graph.EMap.add (src,dest) (0,cap) map)
    Graph.EMap.empty
    capacities

let add_graph_to_console name (graph, layout, source, sink, capacities) =
  Console.add name (
		graph, (create_emap capacities), 0, source, sink,
		(fun vertex -> Graph.VMap.unsafe_find vertex layout)
	      )   


let action_define_capacitated_graph name =
  Console.ask
    (question_capacitated_graph name) (add_graph_to_console name)

let action_change_cap_of_arc name =
  let open State.Infix in
  Console.find name >>= function
    | None ->
       Console.send_messages
	 Tree.(Node ("Undefined graph.", []))
    | Some (graph,emap,flow,source,sink,layout) ->
       let ed_layout = Graph.fold_vertices 
			 ~f:(fun vertex vmap -> 
			  Graph.VMap.add vertex (layout vertex) vmap)
			 graph
			 Graph.VMap.empty
       in
       let editable_graph = EditableGraph.create ~graph ~layout:ed_layout in
       let action_on_question ((src,dest),cap) =
	 add_graph_to_console
	   (Printf.sprintf "%s" name)
	   (graph,
	    ed_layout,
	    source,
	    sink,
	    [(src,dest),cap]
	   )
       in 
       Console.remove name >>
       Console.ask
	(question_set_chosen_arc_capacity 7 editable_graph) 
        action_on_question
					
       
let do_algo_from_name name =
  let open State.Infix in
  Console.find name >>= function
  | None ->
    Console.send_messages
      Tree.(Node ("Undefined graph.",[]))
  | Some (graph,emap,flow,source,sink,layout) ->
    register_algo (graph,emap,layout,source,sink)

let default_graph =
  let open Graph in
  let default_graph =
    Graph.empty
    |> add_vertex 0
    |> add_vertex 1
    |> add_vertex 2
    |> add_vertex 3
    |> add_arc ~src:0 ~dst:1
    |> add_arc ~src:0 ~dst:2
    |> add_arc ~src:1 ~dst:2
    |> add_arc ~src:1 ~dst:3
    |> add_arc ~src:2 ~dst:3
  in
  let default_layout = function
    | 0 -> Gg.P2.v 1.0 1.0
    | 1 -> Gg.P2.v 2.0 2.0
    | 2 -> Gg.P2.v 5.0 2.0
    | 3 -> Gg.P2.v 4.0 3.0
    | _ -> Gg.P2.v 0.0 0.0
  in
  let default_emap = 
    let open Graph.EMap in
    Graph.EMap.empty
    |> add (0,1) (0,4) 
    |> add (0,2) (0,2) 
    |> add (1,2) (0,3)
    |> add (1,3) (0,1)
    |> add (2,3) (0,6)
  in 
  (default_graph,
   default_emap,
   0,
   0,
   3,
   default_layout)   

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

let command_change_cap =
  let open Control.Line in
  Control.Argument.ident
    "change_cap"
    "Change capacity of a chosen edge in the graph"
    @* action_change_cap_of_arc





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
  ++ register
       ~doc:"change cap of edge"
       "change"
       command_change_cap
       





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
    ~before:(
      Console.add "default_graph" default_graph
    )
    (JsContext.init_context ())
