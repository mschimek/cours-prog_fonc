

(**** helpers for chars and strings ****)
let (@@) f g x = x |> g |> f
                 
let string_of_list lst = 
  let n = List.length lst in
  let res = String.create n in
  List.iteri (fun i char -> res.[i] <- char) lst;
  res
let string_of_char c = String.make 1 c
let list_of_string text =
  let array = Array.init (String.length text) (fun i -> String.get text i) in
  Array.to_list array 

let is_in_interval lower upper c = lower <= c && c <= upper
let is_ext_interval lower upper c = is_in_interval lower upper c || c = '_'
let is_figure = is_in_interval '0' '9'
let is_ext_figure = is_ext_interval '0' '9'
let is_lowercase = is_in_interval 'a' 'z'
let is_uppercase = is_in_interval 'A' 'Z'
let is_letter c = is_uppercase c || is_lowercase c
let is_alphanum c = is_letter c || is_figure c || List.mem c ['_';'\'']
let is_hexa c =
  is_figure c
  || is_in_interval 'a' 'e' c
  || is_in_interval 'A' 'E' c
let is_ext_hexa c = is_hexa c || c = '_'
let is_endline c = c = '\n'
  
let is_infix = String.contains "=<>@^|&+-*/$%"
let is_operator = String.contains "!$%&*+-./:<>=?@^|~"
let is_punctuation = String.contains "(){}[];#"

let is_space = String.contains " \t\r\n\x0C"


(* stream monad *)
module Stream = MoreStream




type position =
  { column : int;
    line : int
  }


let compare_position left right =
  if left.line = right.line then left.column - right.column
  else left.line - right.line


type status = Ok | Failed of position
              
type context =
  { position : position;
    input : char Stream.t;
    recognized : char list;
    status : status
  }


let get_position =
  let open State.Infix in
  State.get >>= fun context -> State.return context.position

let get_status =
  let open State.Infix in
  State.get >>= fun context -> State.return context.status


let update_position char =
  let open State.Infix in
  State.get >>= fun context ->
  let new_position =
    if is_endline char then { column = 0; line = context.position.line + 1 }
    else { context.position with column = context.position.column + 1 }
  in
  State.set { context with position = new_position }

let get_first_char =
  let open State.Infix in
  State.get >>= fun ctxt ->
  match Stream.view ctxt.input with
  | Some (head,tail) -> State.return (Some head)
  | None -> State.return None

let remove_first_char =
  State.update_state
    (fun ctxt -> { ctxt with input = Stream.drop 1 ctxt.input })


let recognize char =
  State.update_state
    (fun ctxt -> { ctxt with recognized = char :: ctxt.recognized })
                      



type 'result read = ('result, context) State.m 


let is_success ctxt = ctxt.status = Ok
let position ctxt = ctxt.position 
let remaining_input ctxt = ctxt.input  
let string_read ctxt = ctxt.recognized |> List.rev |> string_of_list
let first_error ctxt = match ctxt.status with
  | Ok -> None
  | Failed pos -> Some pos

let msg_when_incomplete context =
  let open Tree in 
  Node (
    "I cannot read all the input",
    [ singleton
        (Printf.sprintf "I can only read {|%s|}" (string_read context))
    ]
  )


let init_position = { column = 0; line = 0 }

let run ?(offset=init_position) parse input =
  let init_context = 
    { position = offset;
      input;
      recognized = [];
      status = Ok
    }
  in
  parse init_context



(**** Basic parsers ****)
    

let return value = State.return value

let fail =
  let open State.Infix in
  get_status >>= function
  | Ok ->
    get_position >>= fun pos ->
    State.update_state (fun ctxt -> { ctxt with status = Failed pos })
  | Failed pos -> return ()

let consume ~expected ~satisfying =
  let open State.Infix in
  get_first_char >>= function
  | Some c when satisfying c ->
    update_position c >>
    remove_first_char >>
    recognize c >>
    return c
  | otherwise ->
    fail >>
    recognize expected >>
    return expected


let any_char = consume ~expected:'?' ~satisfying:(fun any -> true)

let any_of string =
  if String.length string = 0 then
    raise (Invalid_argument "Parsec.any_of should have non-empty string")
  else
    consume
      ~expected:string.[0]
      ~satisfying:(fun any -> String.contains string any)

let char c = consume ~expected:c ~satisfying:(fun any -> any = c)

(* predicat valid should be true *)
let satisfying ~valid ~predicate =
  consume ~expected:valid ~satisfying:predicate 


let lookahead ~n =
  let open State.Infix in
  State.get >>= fun ctxt -> return (Stream.take n ctxt.input)
  


(**** Basic combinators ****)



(* biased alternative *)
(* the complication here is to take care of erroneous run. If the run
   is already in error, just use the right rule: we just want to find an
   acceptable run. Else try left and if left is in error try right
   with initial context.
   Usually right is used when in error state. This can be changed by setting rev. 
*)
let choose ?(rev=false) left_reader right_reader =
  let open State.Infix in
  get_status >>= function
  | Failed pos when rev -> left_reader
  | Failed pos -> right_reader
  | Ok ->
    begin
      State.get >>= fun initial_context ->
      left_reader >>= fun left_result ->
      get_status >>= function
      | Failed pos -> State.set initial_context >> right_reader
      | Ok -> return left_result
    end
 

(* bind *)
let bind = State.bind 


(* functor *)
let rec map ~f reader =
  bind reader (fun res -> return (f res))
                                
(* applicative *)
let rec apply fct_reader arg_reader =
  bind fct_reader
    (fun fct -> bind arg_reader
        (fun arg -> return (fct arg)))



(* Seems to be useless *)
(* try to read, inverse the status and leaves the context
   unchanged. This is used to check that some parser does not work on the
   input *)
(* let negate reader = *)
(*   let open State.Infix in *)
(*   State.get >>= fun init_ctxt ->  *)
(*   reader >>= fun result -> *)
(*   State.get >>= fun final_ctxt -> *)
(*   State.set init_ctxt >> *)
(*   if final_ctxt.status = Ok then fail *)
(*   else return () *)
      

let exact_string string = 
  let char_list = list_of_string string in
  let cons c list_reader =
    apply 
      (map ~f:(fun cread list -> cread::list) (char c))
      list_reader
  in
  map 
    ~f:string_of_list
    (List.fold_right cons char_list (return []))

(* a version of Infix where spaces are not automatically ignored *)
module InfixNoSpace = struct
  let (>>=) = bind
  let (+>) = apply
  let (||>) rd1 rd2 = choose ~rev:false rd1 rd2
  let ( *> ) f m = map ~f m
      
  let (++>) reader string = reader +> exact_string string
      
  let ( *+> ) f string = f *> exact_string string
                           
end

open InfixNoSpace

(**** Composite combinators ****)

let rec many reader =
  ( reader >>= fun head ->
    many reader >>= fun tail ->
    return (head::tail)
  )
  ||> return []

let at_least_one reader =
  (fun head tail -> head::tail) *> reader +> many reader

let option reader =
  (fun res -> Some res) *> reader
  ||> return  None


let sep_by ~sep reader =
  let sepAndReader = (fun _ res -> res) *> sep +> reader in
  (fun hd tl -> hd::tl) *> reader +> many sepAndReader
  ||> return []


let space = satisfying ~valid:' ' ~predicate:is_space
let spaces =
  string_of_list *> many space

  
let ignore_spaces reader =
  (fun sp res -> res) *> spaces +> reader

module Infix = struct
  let (>>=) reader fct =
    reader >>= fun res ->
    spaces >>= fun _spaces ->
    fct res

  let (+>) reader_fct reader_arg =
    reader_fct >>= fun fct ->
    spaces >>= fun _spaces ->
    reader_arg >>= fun arg -> return (fct arg)

  let ( *> ) fct reader_arg =
    spaces >>= fun _spaces ->
    fct *> reader_arg


  let (||>) reader1 reader2 =
    spaces >>= fun _spaces ->
    reader1 ||> reader2

  let (++>) reader string =
    reader +> exact_string string

  let ( *+>) fct string =
    fct *> exact_string string

  let many reader = many (ignore_spaces reader)
  let sep_by ~sep reader =
    sep_by ~sep:(ignore_spaces sep) (ignore_spaces reader)
  
end


(**** reading tokens ****)

let figure = satisfying ~valid:'0' ~predicate:is_figure
let ext_figure = satisfying ~valid:'0' ~predicate:is_ext_figure
let letter = satisfying ~valid:'a' ~predicate:is_letter
let ident_char = satisfying ~valid:'_' ~predicate:is_alphanum
let hexadecimal = satisfying ~valid:'0' ~predicate:is_hexa
let operator_char = satisfying ~valid:'+' ~predicate:is_operator


let read_positive_int =
  let read_figures predicate =
    (fun _ hd tl -> hd::tl)
    *> many (char '_')
    +> satisfying ~valid:'0' ~predicate
    +> many (satisfying ~valid:'0' ~predicate ||> char '_')
  in
  let convert zero code figures =
    '0'::code::figures |> string_of_list |> int_of_string
  in
      (convert *+> "0" +> any_of "xX" +> read_figures is_hexa)
  ||> (convert *+> "0" +> any_of "oO" +> read_figures (is_in_interval '0' '7'))
  ||> (convert *+> "0" +> any_of "bB" +> read_figures (is_in_interval '0' '1'))
  ||> ((int_of_string @@ string_of_list) *> read_figures is_figure)


let read_int =
  (fun _ positive -> - positive) *+> "-" +> read_positive_int
  ||> read_positive_int
 
let read_ident =
  let is_start_ident c = is_letter c || c = '_' in
  let to_string letter chars = string_of_list (letter::chars) in
  to_string
  *> satisfying ~valid:'_' ~predicate:is_start_ident
  +> many ident_char
    

let read_mantisse =
  (fun first integers dot fracs -> first::integers@('.'::fracs)) *>
  satisfying ~valid:'0' ~predicate:is_figure
  +> many ext_figure
  ++> "." 
  +> many ext_figure


let read_exponent =
  (fun e sign first figures -> e::sign::first::figures) *>
  any_of "eE"
  +> (any_of "+-" ||> return '+')
  +> figure
  +> many ext_figure
  
let read_positive_float =
  let make_float mantisse exponent =
    mantisse@exponent |> string_of_list |> float_of_string 
  in
  make_float
  *> read_mantisse 
  +> (read_exponent ||> return ['e';'+';'0'])

let read_float =
  (fun _ positive -> 0. -. positive) *+> "-" +> read_positive_float
  ||> read_positive_float


let int_of_figure = function
  | c when is_figure c -> int_of_char c - int_of_char '0'
  | c when is_in_interval 'a' 'e' c -> int_of_char c - int_of_char 'a' + 10
  | c when is_in_interval 'A' 'E' c -> int_of_char c - int_of_char 'a' + 10
  | _ -> assert false

let read_special_char =
  let hexa_to_char c1 c2 =
    char_of_int (16 * int_of_figure c1 + int_of_figure c2)
  in
  let deci_to_char c1 c2 c3 =
    char_of_int (100 * int_of_figure c1
                 + 10 * int_of_figure c2
                 + int_of_figure c3
                )
  in
  any_char >>= fun first -> 
  begin match first with
  | 'x' -> hexa_to_char *> hexadecimal +> hexadecimal
  | c1 when is_figure c1 -> deci_to_char c1 *> figure +> figure
  | '\\' | '\"' | '\'' -> return first
  | ' ' -> return '\ '
  | 'n' -> return '\n'
  | 't' -> return '\t'
  | 'b' -> return '\b'
  | 'r' -> return '\r'
  | _ -> fail >>= fun () -> return '*'
  end

let non_escape =
  satisfying ~valid:'*' ~predicate:(fun c -> not (List.mem c ['\\';'\'';'\"']))

let read_single_char = 
  ((fun _ c -> c) *+> "\\" +> read_special_char)
  ||> non_escape

let read_char = 
  (fun _ char _ -> char) *+> "\'" +> any_char ++> "\'"


let read_char_in_string =
      (fun _ _ -> []) *+> "\\\n" +> spaces
  ||> (fun _ x -> [x]) *+> "\\" +> read_special_char
  ||> (fun c -> [c]) *> non_escape

let read_string =
  (fun _ chars _ -> string_of_list (List.flatten chars)) *+>
  "\"" +> many read_char_in_string ++> "\""



let read_symbol = string_of_list *> at_least_one operator_char

let read_keyword = exact_string 
  
let read_punc = satisfying ~valid:';' ~predicate:is_punctuation
    

let rec read_started_comment () =
  get_status >>= fun status ->
  lookahead ~n:2 >>=  function
  | any when status <> Ok -> 
    (fun cl -> [cl]) *> exact_string "*)"
  | ['*';')'] ->
    (fun cl -> [cl]) *> exact_string "*)"
  | ['(';'*'] ->
    (fun op end1 end2 -> op :: end1 @ end2)
    *> exact_string "(*" +> read_started_comment () +> read_started_comment ()
  | '\"'::tl ->
    (fun content comment -> "\"" :: content :: "\"" :: comment)
    *> read_string +> read_started_comment ()
  | otherwise ->
    (fun char tl -> String.make 1 char :: tl)
    *> any_char +> read_started_comment ()
    

let read_comment =
  (fun op ends -> String.concat "" (op::ends)) *>
  exact_string "(*" +> read_started_comment ()
  


let read_dummy =
  (^) *> spaces +> read_comment
  ||> spaces
  
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

let keywords = function
  | [] -> raise (Invalid_argument "Parsec.keywords: no keyword defined")
  | first::words ->
    words
    |> List.map exact_string
    |> List.fold_left (||>) (exact_string first)
    |> map ~f:(fun kw -> Keyword kw)
      
let lexer_without_keywords =
  (fun str -> Ident str) *> read_ident
  ||> (fun str -> Symbol str) *> read_symbol
  ||> (fun str -> String str) *> read_string
  ||> (fun fl -> Float fl) *> read_float
  ||> (fun i -> Int i) *> read_int
  ||> (fun c -> Char c) *> read_char
  ||> (fun p -> Punc p) *> read_punc

let lexer words =
  (fun dum tok -> tok) *> read_dummy +> 
  if words = [] then
    lexer_without_keywords
  else
    begin
      keywords words ||> lexer_without_keywords
    end

