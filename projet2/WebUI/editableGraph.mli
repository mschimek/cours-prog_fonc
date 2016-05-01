type t
type layout = Gg.p2 Graph.VMap.t

val create : graph:Graph.t -> layout:layout -> t 

val set_graph : Graph.t -> layout -> t -> t
val set_layout : layout -> t -> t

val get_graph : t -> Graph.t
val get_layout : t -> layout
val get_canvas_elt : t -> Dom_html.canvasElement Js.t
    


val redraw :
  ?more_arcs:(Gg.p2 * Gg.p2) list
  -> ?more_vertices:Gg.p2 list
  -> t -> t


type element =
  | Vertex of Graph.vertex
  | Arc of Graph.arc
  | Nothing

val closest_element : Gg.p2 -> t -> element
val closest_vertex : ?except:Graph.vertex -> Gg.p2 -> t -> Graph.vertex option 
val closest_arc : Gg.p2 -> t -> Graph.arc option 



val set_vertex_selected : Graph.vertex -> t -> t
val remove_vertex_selected : Graph.vertex -> t -> t

val set_arc_selected : Graph.arc -> t -> t
val remove_arc_selected : Graph.arc -> t -> t


val set_vertex_properties :
  [`Vertex] MoreImage.property list -> Graph.vertex -> t -> t
val remove_vertex_properties :
  ([`Vertex] MoreImage.property list -> [`Vertex] MoreImage.property list) list
  -> Graph.vertex -> t -> t 


val set_arc_properties :
  [`Arc] MoreImage.property list -> Graph.arc -> t -> t
val remove_arc_properties :
  ([`Arc] MoreImage.property list -> [`Arc] MoreImage.property list) list
  -> Graph.arc -> t -> t 



val edit_graph_form : t -> t Question.form
    
val select_vertices_form : t -> (Graph.VSet.t * t) Question.form
    
val select_arcs_form : t -> (Graph.ESet.t * t) Question.form
    
val select_single_vertex_form :
  Graph.vertex -> t -> (Graph.vertex * t) Question.form
    
val select_single_arc_form :
  Graph.arc -> t -> (Graph.arc * t) Question.form




