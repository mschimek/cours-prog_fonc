module Make :
  functor (Context : BasicTypes.ObservationContext) ->
  functor (Emulator : BasicTypes.Emulator
           with type context = Context.context
          ) ->
  functor (Value : MoreModules.DrawableType) ->
sig

  type value
  type context
  type 'any t = ('any,context) State.m
  
  (* val initial_context : context *)


  (** {3 Managing the console output text area} *) 

  (** removes all messages displayed *)
  val empty_messages : unit t


  (** replaces messages displayed, structure the text using the tree *)
  val send_messages : (string Tree.non_empty_tree) -> unit t 



  (** {3 Managing the dictionary of values} *)

  (** [add key value] adds or replaces a key *)
  val add : string -> value -> unit t

  (** [find key] returns the value attached to [key] *)
  val find : string -> value option t

  (** [remove key] *)
  val remove : string -> unit t 

  (** [rename_value old_key new_key] replaces the previous binding 
      from [old_key] by a binding from [new_key], with same value attached 
  *)
  val rename_value : string -> string -> unit t


  (** {3 Managing the input area} *)


  (** replaces text inside the input area *)
  val set_input : string -> unit t
      

  (** retrieves text from the input area *)
  val get_input : string t
      

  (** {3 Effects} *)
  
  (** Fires an effect *)
  val act : Context.effect -> unit t



  (** {3 Questions} *)

  val ask : 'answer Question.t -> ('answer -> unit t) -> unit t 

  
  
  (** {3 Managing computations} *)

  (** [register computation continuation] adds a new computation to the
      queue. [continuation] will be called when [computation] gets
      completely computed. *)
  val register : 'result Emulator.t ->
    ('result -> unit t) -> unit t

  (** Executes one step of the current computation (or load the next
      computation) *)
  val execute_next_step : unit t

  (** Executes the current computation to its end *)
  val execute_current_run : unit t

  (** Executes all the computation of the queue until they are over *)
  val execute_all : unit t 


  (** {3 Managing events } *)

  val on_submit_click : unit t -> unit

  val on_step_click : unit t -> unit

  val on_run_click : unit t -> unit

  val on_run_all_click : unit t -> unit

  val start_event_loop : ?before:(unit t) -> Context.context -> unit
  
end
  with type value = Value.t 
                      
