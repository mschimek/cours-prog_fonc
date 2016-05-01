module Self =
struct
  type t =
    { graph : Graph.t;
      layout : Graph.vertex -> Gg.p2;
      reached : Graph.VSet.t;
      frontier : Graph.arc MoreQueue.t;
      tree : Graph.ESet.t;
    }


  let vertex_properties ctxt vertex =
    let open MoreImage in
    if Graph.VSet.mem vertex ctxt.reached then
      Properties.([filling_color Gg.Color.black])
    else
      Properties.([
          drawing_color (Gg.Color.gray 0.8);
          filling_color (Gg.Color.white)
        ])


  let arc_properties ctxt ((src,dest) as arc) =
    let open MoreImage in 
    if Graph.ESet.mem arc ctxt.tree then
      Properties.([drawing_color Gg.Color.red;
                   linewidth 0.02])
    else
      let is_in_frontier =
        MoreQueue.fold_left
          ~f:(fun bool frontier_arc ->
              bool || arc = frontier_arc)
          false
          ctxt.frontier
      in
      if is_in_frontier then
        Properties.([ drawing_color (Gg.Color.blue);
                      filling_color (Gg.Color.blue);
                    ])
      else
        Properties.([drawing_color (Gg.Color.gray 0.8);
                     filling_color (Gg.Color.gray 0.8);
                    ])



  let to_image ctxt =
    MoreImage.draw_graph
      ~vertex_properties:(vertex_properties ctxt)
      ~arc_properties:(arc_properties ctxt)
      ~layout:ctxt.layout
      ctxt.graph
end


type t = Self.t
open Self



module Show = JsContext.AddDrawable(Self)

let show_bfs_context ctxt =
  JsContext.compose
    (Show.show ~width:120 ~height:90 ctxt)
    (JsContext.end_line)
    




let rec bfs_loop
    ({graph; layout; reached; frontier; tree} as ctxt) =
  let open JsEmulation.Computation in
  let open JsEmulation.Computation.Infix in
  pay >>
  match MoreQueue.observe_left frontier with
  | None  ->
    observe (JsContext.msg "Frontier is empty.") >>
    return ctxt.tree
  | Some (arc,tail) ->
    let (src,dest) = arc in
    let msg = 
      if Graph.VSet.mem dest reached then
        Printf.sprintf
          "Arc (%d,%d): Destination is known."
          src dest
      else    
        Printf.sprintf "Arc (%d,%d): Discover vertex."
          src dest
    in
    observe (show_bfs_context ctxt) >>
    observe (JsContext.msg msg) >>
    if Graph.VSet.mem dest reached then
      bfs_loop { ctxt with frontier = tail }
    else
      let new_frontier =
        Graph.ESet.fold
          (fun elt queue -> MoreQueue.add_right queue elt)
          (Graph.delta_out dest graph)
          tail
      in
      bfs_loop
        { ctxt with
          reached = (Graph.VSet.add dest reached);
          frontier = new_frontier;
          tree = (Graph.ESet.add arc tree);
        }


let bfs ~graph ~layout src =
  let init_ctxt =
    { graph;
      layout;
      reached = Graph.VSet.singleton src;
      tree = Graph.ESet.empty;
      frontier =
        Graph.ESet.fold
          (fun arc queue -> MoreQueue.add_right queue arc)
          (Graph.delta_out src graph)
          MoreQueue.empty;
    }
  in
  let open JsEmulation.Computation.Infix in
  JsEmulation.Computation.observe
    (show_bfs_context init_ctxt) >>
  bfs_loop init_ctxt


