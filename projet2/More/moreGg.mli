val distance : Gg.v2 -> Gg.v2 -> float

module Line : sig
  type t

  val of_points : Gg.p2 -> Gg.p2 -> t

  val of_point_and_normal : Gg.p2 -> Gg.v2 -> t

  val distance_to : point:Gg.p2 -> t -> float

  val projection_onto : point:Gg.p2 -> t -> Gg.p2

  val normal : t -> Gg.v2

  val tangent : t -> Gg.v2

  val mem : Gg.p2 -> t -> bool 
end


module Segment : sig
  type t

  val of_points : Gg.p2 -> Gg.p2 -> t

  val gate : point:Gg.p2 -> t -> Gg.p2

  val distance_to : point:Gg.p2 -> t -> float 

end
