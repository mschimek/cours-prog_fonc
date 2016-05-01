(** The type of parsers reading a ['result] *)
type 'result read
(** those parsers always read something, creating characters if they
   can't find a way to parse. In practice, if the text has broken
   syntax, it will read a correct text with a longuest possible common
   prefix.

   This also means no effect should be done at parsing time, or some
   would be fired by error when the parser try to combine a correct
   sentence. DO NOT USE EFFECTS!

*)

type position =
  { column : int;
    line : int;
  }
  
(** the type of parsing context, containing several informations *)
type context

val run : ?offset:position -> 'result read -> char MoreStream.t -> 'result * context

(** returns whether no error occurs during the parsing process *)
val is_success : context -> bool

(** returns the position of the first unread symbol of the input *) 
val position : context -> position

(** returns the position of the first error encountered *)
val first_error : context -> position option

(** returns the characters read by the parser, as a string *)
val string_read : context -> string 

(** returns what is left of the input. *)
val remaining_input : context -> char MoreStream.t

(** error message when parsing could not read all the source. *)
val msg_when_incomplete : context -> string Tree.non_empty_tree



(** {3 Basic parsers} **)

val return : 'result -> 'result read

val fail : unit read


(** reads the next char *)
val any_char : char read

(** reads the next char if it is the one given, or fail *)
val char : char -> char read

(** reads one of the char contained by the string *)
val any_of : string -> char read 
    
(** A general function to read a character, satisfying some property.  
   [valid] is a character that can be read by this parser, it will be
   chosen by the parser if the current character does not satisfy the
   predicate.
*)
val satisfying : valid:char -> predicate:(char -> bool) -> char read 

(** returns the up to [n] next characters, without consuming them. 
   Typical use is to choose which rule to apply, based on the beginning 
   of the following sequence to parse. 
*)
val lookahead : n:int -> char list read 





(** {3 Basic combinators} *)

val map : f:('value -> 'image) -> 'value read -> 'image read

val apply : ('value -> 'image) read -> 'value read -> 'image read

(** Biased alternative: left choice is taken if possible.  If the
    parser detected an error, it will always ignore the left choice.
    [rev] can be set to ignore the right choice instead. By default,
    the rightmost choice should always be non-recursive, or the
    parsing will not terminate.

    (* The reason is that the parsers always succeed, so an infinite
    derivation would be considered, even if the input does not
    match. Here the solution adopted is stop choosing when in error and
    try to complete the parsing as fast as possible, by taking the
    alternative derivations (by default the rightmost derivations). 
    *)

 *)
val choose : ?rev:bool -> 'result read -> 'result read -> 'result read

val bind : 'value read -> ('value -> 'image read) -> 'image read


(* reads the string given character by character. *)
val exact_string : string -> string read

(* Functions in [InfixNoSpace] take spaces into account. These
   functions should be used to write parsers for elementary tokens. 
*)
module InfixNoSpace : sig

   (** bind *)
  val (>>=) : 'value read -> ('value -> 'image read) -> 'image read

  (** apply *)
  val (+>) : ('value -> 'image) read -> 'value read -> 'image read

  (** map *)
  val ( *> ) : ('value -> 'image) -> 'value read -> 'image read

  (** choose *)
  val (||>) : 'result read -> 'result read -> 'result read

  (** apply with a string reader *)
  val (++>) : (string -> 'image) read -> string -> 'image read 

  (** map with a string reader *)
  val ( *+> ) : (string -> 'image) -> string -> 'image read

end


(* Functions in Infix ignore spaces, so they should be used to write
   parsers above the token level.
*)
module Infix : sig
  (** bind *)
  val (>>=) : 'value read -> ('value -> 'image read) -> 'image read

  (** apply *)
  val (+>) : ('value -> 'image) read -> 'value read -> 'image read

  (** map *)
  val ( *> ) : ('value -> 'image) -> 'value read -> 'image read

  (** choose *)
  val (||>) : 'result read -> 'result read -> 'result read

  (** apply with a string reader *)
  val (++>) : (string -> 'image) read -> string -> 'image read 

  (** map with a string reader *)
  val ( *+> ) : (string -> 'image) -> string -> 'image read

  (** this version of [many] ignores spaces *)
  val many : 'result read -> 'result list read

  (** this version of [sep_by] ignores spaces *)
  val sep_by : sep:('sep read) -> 'result read -> 'result list read
end


(** {3 Composite combinators} *)

(** this version of [many] consider spaces as normal characters. *)
val many : 'elt read -> 'elt list read
    
val at_least_one : 'elt read -> 'elt list read
    
val option : 'elt read -> 'elt option read

(** this version of [sep_by] consider spaces as normal characters. *)
val sep_by : sep:'sep read -> 'elt read -> 'elt list read


(** reads one space *)
val space : char read

(** reads as many spaces as possible, possibly none *)
val spaces : string read
    
(** reads spaces, then reads with the argument *)
val ignore_spaces : 'any read -> 'any read 

(** {3 Reading tokens} *)

(** The next functions read tokens with ocaml lexical conventions. *)

val read_int :    int read
val read_ident :  string read
val read_float :  float read
val read_char :   char read
val read_string : string read
val read_symbol : string read
val read_keyword : string -> string read (* = exact_string *)
val read_comment : string read
val read_punc : char read
    
type token =
  | Int of int
  | Float of float
  | Char of char
  | Punc of char 
  | String of string
  | Keyword of string
  | Ident of string
  | Symbol of string
  | Eof


(** [lexer keywords] reads a token, either a keyword given in
    argument, or one of the other possible tokens. any space or
    comment before the token is ignored. *)
val lexer : string list -> token read
