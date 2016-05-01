module Argument = struct
  
  type 'kind t =
    { descr : string;
      name : string;
      label : string option;
      reader : 'kind Parsec.read;
      default : 'kind option
    }

  
  let add_default_to_parser ?default reader =
    let open Parsec.Infix in
    match default with
    | None -> Parsec.ignore_spaces reader
    | Some (default,as_string) ->
      (function None -> default | Some value -> value)
      *> Parsec.ignore_spaces (Parsec.option reader)

  
  let add_label_to_parser ?label reader =
    let open Parsec.Infix in
    match label with
    | Some name ->
      (fun _ _ arg -> arg)
      *> Parsec.exact_string name +> Parsec.char ':' +> reader
    | None -> reader

  
  let make_descr ?default name descr =
    match default with
    | None -> 
      Printf.sprintf "%s: %s" name descr
    | Some (default,as_string) ->
      Printf.sprintf "%s: %s (default value: %s)" name descr as_string

  
  let argument ?label ?default ~name ~descr basic_reader=
    let reader =
      basic_reader
      |> add_default_to_parser ?default
      |> add_label_to_parser ?label
    in
    let descr = make_descr ?default name descr in
    let default = MoreOption.Infix.(default >>= fun (d,str) -> Some d) in
    { name; descr; label; reader; default  }


  let int ?label ?default name descr =
    let default =
      MoreOption.Infix.(default >>= fun v -> Some (v, string_of_int v))
    in
    argument ?label ?default ~name ~descr Parsec.read_int 

  
  let string ?label ?default name descr =
    let default =
      MoreOption.Infix.(default >>= fun v -> Some (v, v))
    in
    argument ?label ?default ~name ~descr Parsec.read_string

  
  let float ?label ?default name descr =
    let default =
      MoreOption.Infix.(default >>= fun v -> Some (v, string_of_float v))
    in
    argument ?label ?default ~name ~descr Parsec.read_float

  let ident ?label ?default name descr =
    let default =
      MoreOption.Infix.(default >>= fun v -> Some (v, v))
    in
    argument ?label ?default ~name ~descr Parsec.read_ident

  
  let bool ?label ?default name descr =
    let open Parsec.Infix in
    let default =
      MoreOption.Infix.(default >>= fun v -> Some (v, string_of_bool v))
    in
    argument ?label ?default ~name ~descr
      (    (fun _ -> true) *> Parsec.exact_string "true"
       ||> (fun _ -> false) *> Parsec.exact_string "false"
      )

  
  let list_of ?label ?default name element_arg =
    let open Parsec.Infix in
    let reader =
      (fun op list cl -> list)
      *> Parsec.char '['
      +> Parsec.sep_by ~sep:(Parsec.any_of ";,") element_arg.reader
      +> Parsec.char ']'
    in
    argument ?label ?default
      ~name
      ~descr:("List of " ^ element_arg.descr)
      reader

  
  let couple_of ?label ?default name first_arg second_arg =
    let open Parsec.Infix in
    let reader =
      (fun op one comma two cl -> (one,two))
      *> Parsec.char '('
      +> first_arg.reader
      +> Parsec.char ','
      +> second_arg.reader
      +> Parsec.char ')'
    in
    let descr =
      Printf.sprintf "A couple of %s and %s." first_arg.descr second_arg.descr
    in
    argument ?label ?default ~name ~descr reader


  let either ?label ?default name left right =
    { descr = Printf.sprintf "Either %s or %s" left.descr right.descr;
      name;
      label;
      reader = Parsec.Infix.(left.reader ||> right.reader);
      default
    }
  

  let map ~f arg =
    { descr = arg.descr;
      name = arg.name;
      label = arg.label;
      reader = Parsec.map ~f arg.reader;
      default = MoreOption.map f arg.default
    }

  
end


module Line = struct 

  type 'result t = 
    | Return : 'result -> 'result t
    | Bind : 'arg Argument.t * ('arg -> 'result) t -> 'result t
    | Keyword : string * 'result t -> 'result t
    | Either : 'result t * 'result t -> 'result t 
 
  let return result = Return result

  let ( @* ) arg f = Bind (arg, (Return f))

  let ( @+ ) arg line = Bind (arg, line)

  let ( @^) string line = Keyword (string,line)

  let (<+>) line1 line2 = Either (line1,line2)

  type line_descr =
    { synopsis : string list;
      arguments : string list
    }

  let add_synopsis term line_descr =
    { line_descr with synopsis = term :: line_descr.synopsis }

  let add_arguments argument line_descr =
    let name =
      Printf.sprintf "{%s}" argument.Argument.name
    in
    { synopsis = name :: line_descr.synopsis;
      arguments = argument.Argument.descr :: line_descr.arguments;
    }

  let empty_descr = { synopsis = []; arguments = [] }
 
  
  let rec get_descr : 'result . 'result t -> line_descr list =
    function
    | Return _ -> [empty_descr]
    | Bind (arg, line) ->
      List.map (add_arguments arg) (get_descr line)
    | Keyword (kwd, line) ->
      List.map (add_synopsis kwd) (get_descr line)
    | Either (left,right) ->
      List.append (get_descr left) (get_descr right)
    
  let rec read_line : 'result . 'result t -> 'result Parsec.read =
    let open Parsec.Infix in
    function
    | Return res -> Parsec.return res
    | Bind (arg,line) ->
      (fun arg fct -> fct arg) *> Argument.(arg.reader) +> read_line line
    | Keyword (word,line) ->
      (fun kw arg -> arg) *+> word +> read_line line
    | Either (left,right) ->
      read_line left ||> read_line right 

  
end

module Command = struct

  type 'result single =
    { doc : string;
      keyword : string;
      arguments : 'result Line.t;
    }

  type 'result t = 'result single list

  type 'result return =
    { printed : string Tree.non_empty_tree;
      computed : 'result option
    }


  let list_command commands =
    let command_names =
      List.map (fun cmd -> cmd.keyword) commands
    in
    Tree.Node (
      "  Possible commands are:",
      List.map Tree.singleton ("help"::command_names)
    )
  
  let generate_single_man command =
    let format_single_descr line_descr =
      let args = command.keyword :: line_descr.Line.synopsis in
      let synopsis = Printf.sprintf "synopsis: %s" (String.concat "" args) in
      let arguments = line_descr.Line.arguments in
      if arguments = [] then Tree.Node (synopsis,[])
      else Tree.Node (
          synopsis,
          [Tree.Node ("Arguments: ", List.map Tree.singleton arguments)]
        )
    in
    Tree.Node (
      Printf.sprintf "%s: %s" command.keyword command.doc,
      command.arguments
      |> Line.get_descr 
      |> List.map format_single_descr
    )
      

  let generate_main_help commands =
    Tree.Node (
      "help <command>: prints the documentation of the command.",
      [list_command commands]
    )
  
  let register ~doc keyword arguments =
    { doc; keyword; arguments }


  let empty = []

  let (++) commands single = single::commands


  
  let execute_line line offset input =
    let (result, context) =
      Parsec.run ~offset (Line.read_line line.arguments) input
    in
    let remaining_input = Parsec.remaining_input context in
    let not_space c = not (String.contains " \n\t\r" c) in
    if MoreStream.find not_space  remaining_input <> None then
      let msg = "Could not completely parse the command" in
      let parsed =
        Printf.sprintf "I can only read {|%s %s|}."
          line.keyword
          (Parsec.string_read context)
      in
      { printed = Tree.(Node (msg,[Node (parsed,[])]));
        computed = None;
      }
    else if Parsec.is_success context then
      { printed = Tree.singleton "Successfully interpreted";
        computed = Some result
      }
    else
      let msg = "Could not parse the command. This is the best I can do:" in
      let parsed =
        Printf.sprintf "{|%s %s|}" line.keyword (Parsec.string_read context)
      in
      let error_position =
        Printf.sprintf "First error spotted in line %d, column %d"
          Parsec.((position context).line)
          Parsec.((position context).column)
      in
      { printed =
          Tree.Node (
            "Error",
            Tree.singleton msg
            :: Tree.singleton parsed
            :: Tree.singleton error_position
            :: [generate_single_man line]
          )
        ;
        computed = None
      }

  let execute_help commands input =
    let (result, context) = Parsec.(run (ignore_spaces read_ident) input) in 
    match MoreList.find (fun line -> line.keyword = result) commands with
    | _ when result = "help" ->
      { printed = Tree.singleton "Display this help";
        computed = None
      }
    | None ->
      { printed =
          Tree.Node
            ( "Please provide a command name after \"help\", chosen among:",
              [list_command commands]
            );
        computed = None
      }
    | Some line ->
      { printed = generate_single_man line;
        computed = None;
      }
            
  
  let execute commands input =
    let (result, context) = Parsec.(run (ignore_spaces read_ident) input) in
    if Parsec.is_success context && result = "help" then
      execute_help commands (Parsec.remaining_input context)
    else
      match MoreList.find (fun line -> line.keyword = result) commands with
      | _ when not (Parsec.is_success context) ->
        { printed = generate_main_help commands;
          computed = None
        }
      | None ->
        { printed =
            Tree.Node (
              "Cannot recognize any available command",
              [list_command commands]
            );
          computed = None
        }            
      | Some line ->
        execute_line line
          (Parsec.position context)
          (Parsec.remaining_input context)


      
  


end
