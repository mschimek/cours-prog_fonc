
module type PrintableType = sig
  type t
  val to_string : t -> string
end


module type OrderedType = sig
  type t
  val compare : t -> t -> int
end


module type OrderedAndPrintableType = sig
  type t
  include PrintableType with type t := t 
  include OrderedType with type t := t
end


module type DrawableType = sig
  type t
  val to_image : t -> Vg.image * Gg.box2
end
