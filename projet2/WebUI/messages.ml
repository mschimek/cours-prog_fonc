type message = string Tree.non_empty_tree

let empty_messages div =
  div##childNodes
  |> Dom.list_of_nodeList
  |> List.iter (Dom.removeChild div);
  div##innerHTML <- Js.string ""


let rec format (Tree.Node (string,subtrees)) =
  let node = Dom_html.createDiv Dom_html.document in
  let span = Dom_html.createSpan Dom_html.document in 
  span##innerHTML <- Js.string string;
  span##className <- Js.string "msg";
  Dom.appendChild node span;
  node##className <- Js.string "msg_node";
  subtrees
  |> List.map format
  |> List.iter (Dom.appendChild node);
  node


let update_messages div message =
  empty_messages div;
  Dom.appendChild div (format message)
