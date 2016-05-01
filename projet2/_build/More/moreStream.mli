(** An encoding of streams : a generalization of lists to have
    possibly infinite length *)

(** {3 Definitions }*)


(** The type of finite or infinite lists *)
type 'a t

(** An exception raised when trying to get an element of an empty
    stream *)
exception End_of_stream

(** The empty stream *)
val empty : 'a t

(** Checks if a stream is empty *)
val is_empty : 'a t -> bool


(** {3 Views} *)

(** [view stream] is [None] if [stream] is empty, [Some (first,tail]]
    otherwise, with [first] being the first element of the stream, and
    [others] is the tail of the stream. *)
val view : 'a t -> ('a * 'a t) option

(** [first stream] is the first element of [stream]. Raises
    [End_of_stream] if [stream] is empty. *)
val first : 'a t -> 'a

(** [tail stream] is the tail of [stream]. Raises [End_of_stream] if
    [stream] is empty. *)
val tail : 'a t -> 'a t

(** [to_list stream] is the list of elements in [stream], in the same order. 
    Does not terminate if [stream] has infinite length *)
val to_list : 'a t -> 'a list



(** {3 Construction} *)

(** [repeat elt] is an infinite stream with every element being
    [elt]. *)
val repeat : 'a -> 'a t

(** [iterate f init] is the stream whose [k]th element is the [k]th
    power of [f] applied to [init]. *)
val iterate : ('a -> 'a) -> 'a -> 'a t

(** [from_generator fct init] is the stream obtained by looping [fct]
    on its first output, starting from [init], and collecting the
    second output in the stream. *)
val from_generator : gen:('a -> ('a * 'b) option) -> 'a -> 'b t


(** [from_index fct] is the stream whose [k]th element is [fct k] *)
val from_index : (int -> 'a) -> 'a t


(** [from_list] converts a list to a stream, the order of the elements
    is preserved. *)
val from_list : 'a list -> 'a t

(** [from_array] converts an array to a stream, starting from index
    0. *)
val from_array : 'a array -> 'a t

(** [from_string] converts a string to a stream of characters,
    starting from index 0. *)
val from_string : string -> char t


(** [from_in_channel chan] is the stream of characters read from a
    channel [chan]. *)
val from_in_channel : in_channel -> char t

(** [add elt stream] is the stream whose first element is [elt], and
    whose tail is [stream]. *)
val add : 'a -> 'a t -> 'a t

(** [append left right] appends two streams, preserving the order,
    with the elements of [left] before the elements of [right].  For
    performance, should be associated to the right in priority.
*)
val append : 'a t -> 'a t -> 'a t


(** [cycle stream] repeats periodically [stream] infinitely often. *)
val cycle : 'a t -> 'a t


(** {3 Simple manipulations } *)

(** [take n stream] is the list of [n] first elements in [stream].
    Returns all elements, if [steam] has less than [n] elements.
*)
val take : int -> 'a t -> 'a list


(** [drop n stream] is the stream obtained by removing the [n] first
    elements of [stream]. Returns the empty stream if [stream] has
    less than [n] elements. *)
val drop : int -> 'a t -> 'a t


(** [kth k stream] is the [k]th element of stream.  Raises
    [End_of_stream] if [stream] does not containe [k] elements. *)
val kth : int -> 'a t -> 'a


(** [break_when pred stream] returns as a first term a maximum prefix
    of [stream] whose elements do not check [pred], and as a second
    term the complementing suffix.  May not terminate if [stream] is
    infinite and no element checks [pred].
*)
val break_when : ('a -> bool) -> 'a t -> ('a list * 'a t)


(** [drop_until pred stream] is the second term of [break_when pred
    stream]. *)
val drop_until : ('a -> bool) -> 'a t -> 'a t

(** [keep_until pred stream] is the first term of [break_when pred
    stream]. *)
val keep_until : ('a -> bool) -> 'a t -> 'a list

(** [find pred stream] returns [Some elt] where [elt] is the first
    element of [stream] that checks [pred]. It returns [None] if the
    stream is finite, but no element checks [pred]. It does not
    terminate if the stream is infinite, but no element checks
    [pred]. *)
val find : ('a -> bool) -> 'a t -> 'a option


(** {3 Higher order functions} *)


(** [map] maps. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [scan fct init stream] folds [fct] from left to right, and returns
    the stream of partial results. *)
val scan : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t

(** [filter pred stream] is the stream of elements of [stream]
    checking [pred], with order unchanged. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** [partition pred stream] is a couple of the a stream of the element
     checking [pred], and a stream of the elements that do not check
     [pred], with order preserved. *)
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

(** [flatten] appends the streams of a stream. *)
val flatten : 'a t t -> 'a t



(** {3 Function on two streams} *)

(** [combine left right] is a stream whose kth element is a pair of
    the kth elements of left and right respectively.  Its length is
    the minimum length of [left] and [right].
*)
val combine : 'a t -> 'b t -> ('a * 'b) t

(** [split stream] is the streams of first term and second term of
    each element of [stream] with order preserved. *)
val split : ('a * 'b) t -> 'a t * 'b t


(** [map2 fct left right] is a sream whose kth element is [fct i j]
    where [i] and [j] are the kth elements of [left] and [right]
    respectively.  Its length is the minimum length of [elft] and
    [right].  *)
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t



(** {3 Stream monad} *)

module Monad : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
   
  (** [return a] is a stream with a single element, [a] *)
  val return : 'a -> 'a t
end


(** {3 Infix operators} *)

module Infix : sig
  (** same as [append] *)
  val (@@) : 'a t -> 'a t -> 'a t

  (** same as [add] *)
  val (++) : 'a -> 'a t -> 'a t

  (** same as [kth] *)
  val (!!) : int -> 'a t -> 'a

  (** same as [combine] *)
  val ( ** ) : 'a t -> 'b t -> ('a * 'b) t
end
