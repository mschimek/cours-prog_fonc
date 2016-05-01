
module type ObservationContext = sig

  (** This represents a context in which a computation will be
      evaluated. Effects will happen on this context during
      the evaluation. 
  *)

  (** The environment for observing the computation. *)
  type context

  (** An action on the environment, can be thought of as [context -> context]. *)
  type effect

  (** This action does nothing. *)
  val no_effect : effect

  (** Applies the action on the environment. *)
  val apply : effect -> context -> context


  (** The composition of two succesive actions. *)
  val compose : effect -> effect -> effect


  (** This is supposed to switch off the observations. Effects should
      be applied, except when producing an output. *)
  val switch_off : effect

  (** This is supposed to turn on the observations. Effects should be
      applied normally afterwards. It is on by default. *)
  val switch_on : effect

end



module type Computation =
sig

  (** This allows to describe an algorithm as a sequence of several
      computational steps. Such a computation may then be run by an
      emulator (see below). 
  *)

  (** An effect on the observation context (see above). *)
  type effect

  (** The type of a computation with result of type ['return]. *)
  type 'return t


  (** Unsurprisingly computations are a monad. *)
  val bind : 'init t -> ('init -> 'return t) -> 'return t 

  
  (** Applies effects before and after the computation is evaluated. *)
  val nest :
    ?before:effect
    -> ?after:effect
    -> 'return t -> 'return t

  (** An empty computation, that will axt as a breakpoint for the emulator. *)
  val pay : unit t 


  (** A trivial computation *)
  val return : 'return -> 'return t

  (** A computation that only produces an effect on the context when
      evaluated. *)
  val observe : effect -> unit t


  module Infix : sig
    (* free versions *)
    val (>>=) : 'init t -> ('init -> 'return t) -> 'return t
    val (>>) : unit t -> 'return t -> 'return t
  end

end



module type Emulator =
sig

  (** Emulators are in charge of evaluation computations, and applying
      effects on context.      
  *)

  (** The type of context on which the effects are applied during
      evaluation. *)
  type context

  (** The type of a computation returning a value of type ['return],
      see [Computation} above. *)
  type 'return t

  (** The type of an evaluation in progress, computing a value of type
      ['return]. *)
  type 'return run


  (** The result of a single step of calculus *)
  type 'return result =
    | Done of 'return     (** computation is over *)
    | More of 'return run (** must pay to go further *)
    | Free of 'return run (** something happened, computation is not over *)

  
  (** Starts an evaluation *)
  val init_run : 'return t -> 'return run

  (** Does one step of computation. *)
  val step :
    'return run -> context -> 'return result * context

  (** Evaluates until getting a result. *)
  val execute :
    'return t -> context -> 'return * context

end
