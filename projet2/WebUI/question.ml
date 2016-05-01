  
type question =
  { container : Dom_html.divElement Js.t;
    title : Dom_html.divElement Js.t;
  }
  
type 'answer form =
  { display : Dom_html.divElement Js.t;
    get_answer : unit -> 'answer option;
  }

type 'answer t =
  | Single : question * 'answer form -> 'answer t
  | Pack : question * 'answer t -> 'answer t 
  | Conjonction : 'left t * 'right t -> ('left * 'right) t
  | Disjonction : question * 'answer t list -> 'answer t
  | Bind : 'first t * ('first -> 'second t) -> 'second t
  | Map : ('answer -> 'image) * 'answer t -> 'image t
  | Return : 'answer -> 'answer t 


type 'answer prepared =
  | PrepSingle : question * 'answer form -> 'answer prepared
  | PrepPack : question * 'answer prepared -> 'answer prepared 
  | PrepConjonction :
      'left prepared * 'right prepared -> ('left * 'right) prepared
  | PrepDisjonction : question * 'answer prepared list -> 'answer prepared
  | PrepBind :
      'first prepared
      * ('first -> 'second t)
      * 'second prepared Lwt.t
      * Dom_html.divElement Js.t 
    -> 'second prepared
  | PrepMap : ('answer -> 'image) * 'answer prepared -> 'image prepared
  | PrepReturn : 'answer -> 'answer prepared


let container_of ~title =
  let container = Dom_html.(createDiv document) in
  container##className <- Js.string "question_container";
  { container;
    title;
  }

let question_container_of ~title =
  let container = Dom_html.(createDiv document) in
  container##className <- Js.string "question_form";
  { container;
    title;
  }

let form_of_parser reader =
  let display = Dom_html.(createDiv document) in
  let input = Dom_html.(createTextarea document) in
  let messages = Dom_html.(createDiv document) in 
  Dom.appendChild display messages;
  Dom.appendChild display input;
  input##value <- Js.string "";
  let get_answer () =
    let string = Js.to_string (input##value) in
    Messages.empty_messages messages;
    let (result,context) =
      Parsec.run reader (MoreStream.from_string string)
    in
    let not_space c = not (String.contains " \t\n\r" c) in
    if MoreStream.find not_space (Parsec.remaining_input context) <> None then
      begin
        Messages.update_messages messages (Parsec.msg_when_incomplete context);
        None
      end      
    else if Parsec.is_success context then Some result
    else begin
      let best_string =
        Printf.sprintf
          "This is the best I can make: {|%s|}"
          (Parsec.string_read context)
      in
      let new_msg = Tree.(
          Node ("Please provide a valid answer.", [ Node (best_string,[]) ]))
      in
      Messages.update_messages messages new_msg;
      None
    end
  in 
  { display; get_answer}




let question_of_parser ~title ~reader =
  Single (question_container_of title, form_of_parser reader)


let question_of_form ~title ~form =
  Single(question_container_of title, form)

let get_int = question_of_parser ~reader:Parsec.(ignore_spaces read_int)
let get_float = question_of_parser ~reader:Parsec.(ignore_spaces read_float)


let pack ~title question =
  Pack (container_of title, question)

let conjonction  left_question right_question =
  Conjonction (left_question, right_question)

let disjonction ~title questions =
  Disjonction (container_of title, questions)

let map ~f question = Map (f, question)


let bind first second = Bind (first,second)

let return value = Return value

let string_of_list list =
  list
  |> List.map (fun char -> String.make 1 char)
  |> String.concat ""
  

let get_string ~title =
  map
    ~f:string_of_list
    (question_of_parser ~reader:Parsec.(at_least_one any_char) ~title)



let rec prepare : type answer.
  Dom_html.divElement Js.t -> answer t -> answer prepared =
  fun father question ->
  match question with  
  | Single (question, form) ->
    Dom.appendChild question.container question.title;
    Dom.appendChild question.container form.display;
    Dom.appendChild father question.container;
    PrepSingle (question, form)

  | Pack (question, subquestion) ->
    Dom.appendChild question.container question.title;
    let prep_sub = prepare question.container subquestion in
    Dom.appendChild father question.container;
    PrepPack (question, prep_sub)

  | Conjonction (left, right) ->
    let prep_left = prepare father left in
    let prep_right =  prepare father right in
    PrepConjonction (prep_left, prep_right)

  | Disjonction (question, subquestions) ->
    Dom.appendChild question.container question.title;
    let prepared_subs = List.map (prepare question.container) subquestions in
    Dom.appendChild father question.container;
    PrepDisjonction(question, prepared_subs)

  | Bind (first,second) ->
    let prepared_first = prepare father first in
    let msgs = Dom_html.(createDiv document) in
    let button = Dom_html.(createButton ~_type:(Js.string "submit") document) in
    button##innerHTML <- Js.string "next";
    Dom.appendChild father msgs;
    Dom.appendChild father button;
    let rec thread () =
      let open Lwt in 
      Lwt_js_events.click button >>= fun mouse_event ->
      match recover_answer prepared_first with
      | Some answer ->
        remove_prepared father prepared_first;
        Dom.removeChild father button;
        Dom.removeChild father msgs;
        let prepared_second = prepare father (second answer) in
        Lwt.return prepared_second
      | None ->
        thread ()
    in
    PrepBind (prepared_first, second, thread (), msgs)

  | Map (fct,subquestion) ->
    PrepMap (fct, prepare father subquestion)

  | Return answer -> PrepReturn answer

and recover_bind : type answer .
  answer prepared Lwt.t -> Dom_html.divElement Js.t -> answer option =
  fun thread msg_div ->
  let open Lwt in
  match state thread with
  | Return second ->
    Messages.empty_messages msg_div;
    recover_answer second
  | otherwise ->
    Messages.update_messages
      msg_div
      Tree.(Node ("Please provide answers before going on.",[]));
    None
  
and recover_answer : type answer .
  answer prepared -> answer option =
  function
  | PrepSingle (question, form) ->  form.get_answer ()
  | PrepPack (question,subquestion) -> recover_answer subquestion
  | PrepConjonction (left,right) ->
    let left_answer = recover_answer left in 
    let right_answer = recover_answer right in 
    MoreOption.product left_answer right_answer
  | PrepDisjonction (question,list) ->
    list
    |> List.map recover_answer
    |> MoreList.find ~predicate:(fun answer -> answer <> None) 
    |> MoreOption.flatten
  | PrepBind (first,second,thread,msg_div) -> recover_bind thread msg_div
  | PrepMap (fct,subquestion) ->
    MoreOption.map fct (recover_answer subquestion)
  | PrepReturn answer -> Some answer 


and remove_prepared : type answer .
  Dom_html.divElement Js.t -> answer prepared -> unit =
  fun father -> function
    | PrepSingle (question, form) ->
      Dom.removeChild father question.container;
      Dom.removeChild question.container form.display;
      Dom.removeChild question.container question.title

    | PrepPack (question, subquestion) ->
      Dom.removeChild father question.container;
      remove_prepared question.container subquestion;
      Dom.removeChild question.container question.title

    | PrepConjonction (left,right) ->
      remove_prepared father right;
      remove_prepared father left
      
    | PrepDisjonction (question, subquestions) ->
      Dom.removeChild father question.container;
      List.iter (remove_prepared question.container) subquestions;
      Dom.removeChild father question.title

    | PrepBind(first,to_second,thread,msg_div) ->
      begin match Lwt.state thread with
        | Lwt.Return second -> remove_prepared father second
        | otherwise -> remove_prepared father first
      end

    | PrepMap (fct, subquestion) ->
      remove_prepared father subquestion

    | PrepReturn answer -> ()


let propose ~father question =
  let prepared = prepare father question in
  let button = Dom_html.(createButton ~_type:(Js.string "submit") document) in
  button##innerHTML <- Js.string "send";
  Dom.appendChild father button;
  let open Lwt in
  let rec thread () = 
    Lwt_js_events.click button >>= fun mouse_event ->
    match recover_answer prepared with
    | None -> thread ()
    | Some answer ->
      remove_prepared father prepared;
      return answer
  in
  thread ()



let title_of_string string =
  let div = Dom_html.(createDiv document) in
  div##innerHTML <- Js.string string;
  div

let ask_int text =
  get_int ~title:(title_of_string text)

let ask_float text =
  get_float ~title:(title_of_string text)
    
let ask_string text =
  get_string ~title:(title_of_string text)



module Infix = struct
  let ( *> ) f question = map ~f question 

  let ( +> ) first second =
    map
      ~f:(fun (fct,arg) -> fct arg)
      (conjonction first second)
      
  let (>>=) arg fct = bind arg fct
end

open Infix
    
let of_list questions =
  let cons elt list = elt :: list in
  match List.rev questions with
  | [] -> invalid_arg "Question.of_list: empty list"
  | last::previous ->
    List.fold_left 
      (fun nexts current -> cons *> current +> nexts)
      (map (MoreList.Monadic.return) last)
      previous
    
