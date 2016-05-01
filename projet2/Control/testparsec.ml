#directory "/home/guyslain/Projects/Observer/_build/";;
#directory "/home/guyslain/Projects/Observer/_build/More";;
#require "gg";;
#require "vg";;
#load "more_lib.cma";;
#load "moreStream.cmo";;
#load "state.cmo";;
#directory "/home/guyslain/Projects/Observer/_build/Control";;
#load "parsec.cmo";;


open Parsec

                 
let string_of_list lst = 
  let n = List.length lst in
  let res = String.create n in
  List.iteri (fun i char -> res.[i] <- char) lst;
  res

let run_and_show parse string =
  let (res, ctxt) = run parse (MoreStream.from_string string) in
  Printf.printf "%s\n" (if is_success ctxt then "Ok" else "Fail");
  let pos = position ctxt in
  let left =
    ctxt
    |> remaining_input
    |> MoreStream.take 20
    |> string_of_list
  in
  Printf.printf "  Pos: (%d,%d)\n" pos.column pos.line;
  Printf.printf "  Read: {read|%s|read}\n" (string_read ctxt);
  Printf.printf "  Left: {left|%s|left}\n" left;
  res
    

let parse1 = exact_string "Hello"

let _ = run_and_show parse1 "Hello World!"
let _ = run_and_show parse1 "Hallo World!"


let parse2 = many (char 'a')

let _ = run_and_show parse2 "aaabb"

let parse3 =
  let open Parsec.Infix in
  (fun i _ -> i) *> read_int +> char ';'

let _ = run_and_show (many parse3) "12;34;56poi"

let parse_list =
  let open Parsec.Infix in
  (fun op list cl -> list) *>
  char '[' +> sep_by ~sep:(char ';') read_int +> char ']'

let _ = run_and_show parse_list "[12 ; 21;  34; ]"


let _ = run_and_show read_string "\"\\\"word\\\"\"bla"

let parse_option_list =
  let open Parsec.Infix in
  (fun op list cl -> list) *>
  char '[' +> sep_by ~sep:(char ';') (Parsec.option read_string) +> char ']'


let _ = run_and_show parse_option_list
    "[ \"word\"; \"cat\";;;\"dog\"]end"


let  _ = run_and_show read_comment
    "(* babebibobu (* nested *) \"string *)\" stop *) outside"


let tokenizer = lexer ["let";"in";"fun";"function";"match";"with";"begin";"end"]


let read_dummy =
  let open Parsec.Infix in
  (fun com -> com) *> Parsec.option read_comment

let _ = run_and_show read_dummy "123"

let _ =
  run_and_show
    (Parsec.Infix.many
       (lexer ["let";"in";"fun";"function";"match";"with";"begin";"end"])
    )
    "let (>>=) arg fct = match arg with
     | Some thing -> fct thing
     | None -> None"
