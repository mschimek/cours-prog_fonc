let epsilon = ref 1e-10
let set_epsilon eps = epsilon := eps


let distance vec0 vec1 =
  Gg.V2.(norm (sub vec0 vec1))


module Line = struct
  type t = 
    { origin : Gg.v2;
      normal : Gg.v2;
    }


  let normal_of_vec vec =
    Gg.V2.(v (-.(y vec)) (x vec) |> unit)
      
  let vec_of_normal normal = 
    Gg.V2.(v (y normal) (-.(x normal)) |> unit)

  
  let of_points vec0 vec1 =
    { origin = vec0;
      normal = Gg.V2.sub vec1 vec0 |> normal_of_vec;
    }

  
  let of_point_and_normal vec normal =
    { origin = vec;
      normal;
    }

  let distance_to ~point line =
    abs_float Gg.V2.(dot (point - line.origin) line.normal)
    
  let projection_onto ~point line =
    Gg.V2.(point - distance_to ~point line * line.normal)


  let normal line = line.normal
  let tangent line = vec_of_normal line.normal

  let mem point line = distance_to ~point line < !epsilon 
    
  
end

module Segment = struct
  
  type t =
    { line : Line.t;
      origin : Gg.v2;
      destination : Gg.v2;
      length : float
    }

  let of_points vec0 vec1 =
    { line = Line.of_points vec0 vec1;
      origin = vec0;
      destination = vec1;       
      length = Gg.V2.(norm (sub vec1 vec0))
    }

  let gate ~point segment =
    let proj = Line.projection_onto ~point segment.line in
    let tangent = Line.tangent segment.line in
    let position = Gg.V2.(dot (proj - segment.origin) tangent) in
    if position < 0. then segment.origin
    else if position > segment.length then segment.destination
    else proj
  
  let distance_to ~point segment = distance point (gate ~point segment)
  

  
end

type line = Line.t


