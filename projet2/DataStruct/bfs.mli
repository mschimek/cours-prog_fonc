type t

  

module Self: MoreModules.DrawableType
  with type t = t

val bfs : 
  graph:Graph.t
  -> layout:(Graph.vertex -> Gg.p2)
  -> Graph.vertex
  -> Graph.ESet.t JsEmulation.Computation.t 
