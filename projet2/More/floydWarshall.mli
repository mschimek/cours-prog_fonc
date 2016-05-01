module type Semiring = sig
  type t
  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
end


module Make :
  functor (Ring : Semiring) ->
  functor (Elt : Set.OrderedType) ->
  functor (S : Set.S with type elt = Elt.t) ->
  sig

    type closure = S.elt -> S.elt -> Ring.t option

    val transitive_closure :
      d:(S.elt -> S.elt -> Ring.t option)
      -> S.t
      -> closure

  end
