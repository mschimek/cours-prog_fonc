module Argument : sig

  (** the type of an argument in command-line style *)
  type 'kind t


  (** creating a kind of argument expecting type ['value]. 
      Second argument of [default] is a description of the default value. 
      @param name: short_name for the argument
      @param descr: textual description of the argument
  *)
  val argument :
    ?label:string ->
    ?default:('value * string) ->
    name:string ->
    descr:string ->
    'value Parsec.read -> 'value t 


  (** [int ?label ?default short_name long_description] *)
  val int : ?label:string -> ?default:int -> string -> string -> int t

  val float : ?label:string -> ?default:float -> string -> string -> float t

  val string : ?label:string -> ?default:string -> string -> string -> string t

  val bool : ?label:string -> ?default:bool -> string -> string -> bool t

  val ident : ?label:string -> ?default:string -> string -> string -> string t 
  
  val list_of :
    ?label:string ->
    ?default:('value list * string) ->
    string ->
    'value t -> 'value list t

  val couple_of :
    ?label:string ->
    ?default:(('left * 'right) * string) ->
    string ->
    'left t -> 'right t -> ('left * 'right) t

  val either :
    ?label:string ->
    ?default:'value ->
    string ->
    'value t -> 'value t -> 'value t

  val map : f:('value -> 'im) -> 'value t -> 'im t 
      

end



module Line : sig

  (** The type of one-line commands generating a value of type ['result] *)
  type 'result t

  val return : 'result -> 'result t

  (** A typical example of command would be: 
      ["subcommand" @^ arg1 @+ arg2 @+ arg3 @* (fun a3 a2 a1 -> foo)]

  *)
  
  (** binds an action to an argument, giving a one-argument command *)
  val ( @* ) : 'arg Argument.t -> ('arg -> 'result) -> 'result t

  (** add one more argument to a command *)
  val ( @+) : 'arg Argument.t -> ('arg -> 'result) t -> 'result t

  (** add a keyword required at this position of the command *)
  val ( @^ ) : string -> 'result t -> 'result t

  (** introduce an alternative in how to parse the command-line *)
  val (<+>) : 'result t -> 'result t -> 'result t
  
end


module Command : sig
  (** The type of a basic command-line *)
  type 'result single

  (** [register ~doc keyword line] creates a command-line, with
      starting command [keyword] and arguments and action given by
      [line]
  *)
  val register : doc:string -> string -> 'result Line.t -> 'result single



  (** The type for a combination of several command-lines *) 
  type 'result t

  val empty : 'result t
  val (++) : 'result t -> 'result single -> 'result t


  (** The return type of an execution of a command-line. As it may
      fails, the computed value is only optional.  If no value was
      computed, [printed] contains an error message to be
      displayed.
  *)
  type 'result return =
    { printed : string Tree.non_empty_tree;
      computed : 'result option
    }


  (** Runs a composed command-line *)
  val execute : 'result t -> char MoreStream.t -> 'result return 


end
