
(* alias for ImprovedCanvas.create *)
val create :
  canvas:(Dom_html.canvasElement Js.t)
  -> image:Vg.image
  -> box:Gg.box2
  -> size:Gg.v2
  -> ImprovedCanvas.t

val start_event_handlers : ImprovedCanvas.t -> unit
