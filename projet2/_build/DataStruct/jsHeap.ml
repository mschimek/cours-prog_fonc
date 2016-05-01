
module Make =
  functor (Key : MoreModules.OrderedAndPrintableType) ->
  functor (Elt : MoreModules.PrintableType) ->
  struct
             
    type key = Key.t
    let (<) key1 key2 = Key.compare key1 key2 < 0
    type elt = Elt.t

    type node =
      { left_subtree : t;
        key : key;
        value : elt;
        rank : int;
        right_subtree : t
      }
    and t =
      | Node of node
      | Leaf

    

    (* conversion to string and image *)
        
    let to_string heap =
      let rec self = function
        | Leaf -> ""
        | Node { left_subtree; key; value; right_subtree } ->
	  Printf.sprintf "(%s %s[%s] %s)"
            (self left_subtree)
	    (Key.to_string key)
	    (Elt.to_string value)
	    (self right_subtree)
      in match heap with
      | Leaf -> "Empty"
      | _ -> self heap

    let rec to_bintree = function
      | Leaf -> BinTree.Leaf
      | Node { left_subtree; key; right_subtree } ->
        BinTree.Node (to_bintree left_subtree, key, to_bintree right_subtree)
      
    
    let to_image heap =
      let open Vg in let open Gg in 
      let image_of_node (key,pos) =
        MoreImage.circle_node
          ~radius:0.2
          ~center:pos
          ~text:(Key.to_string key)
      in
      match heap with
      | Leaf -> MoreImage.null_image
      | _non_empty ->
        heap
        |> to_bintree
        |> ReingoldTilford.layout
        |> fun (box, layout) ->
        ( ReingoldTilford.image_of_tree ~image_of_node layout,
          box
        )


    
    (* interfacing with observation libs *)
    
    type t_alias = t 
    module Self = struct
      type t = t_alias
      let to_image = to_image
    end
    module FormatHeap = JsFormatter.Make(Self)

    open JsEmulation.Computation
    open JsEmulation.Computation.Infix
    open FormatHeap.Formatter


    let show_msg formatted =
      observe (FormatHeap.msg formatted)

        
    let show_result result =
      show_msg (send ++ "Result is: " +> result) >>
      return result

    let recurse_with_msg ~message computation =
      let (++) = JsContext.compose in 
      JsEmulation.Computation.nest
        ~before:JsContext.(increment ++ FormatHeap.msg message)
        ~after:JsContext.decrement
        (pay >> computation >>= show_result)
      
    



    (* Algorithms *)

    
    let rank = function
      | Leaf -> -1
      | Node { rank } -> rank

    let node left_subtree key value right_subtree =
      let left_rank = rank left_subtree in
      let right_rank = rank right_subtree in
      let (left_subtree,right_subtree) =
        if right_rank > left_rank then (right_subtree,left_subtree)
        else (left_subtree, right_subtree)
      in
      let rank = 1 + min left_rank right_rank in
      pay >> 
      return (Node { left_subtree; key; value; rank; right_subtree })

    let empty = Leaf

    

    let rec merge heap1 heap2 =
      recurse_with_msg
        ~message:(send ++ "Merge " +> heap1 ++ " with " +> heap2)
        begin
          match heap1, heap2 with
          | Leaf, heap
          | heap, Leaf -> return heap
          | Node ({ key=key1 } as node1), Node { key=key2 } when key1 < key2 ->
            merge node1.right_subtree heap2
            >>= node node1.left_subtree key1 node1.value
          | Node node1, Node node2 ->
            merge heap1 node2.right_subtree
            >>= node node2.left_subtree node2.key node2.value
        end


    let single key value =
      Node
        { left_subtree = Leaf;
          key; value; rank = 0;
          right_subtree = Leaf
        }

    let insert key elt heap =
      recurse_with_msg
        ~message:(send ++ "Insert " ++ Key.to_string key ++ " in " +> heap)
        begin
          merge (single key elt) heap
        end

    let minimum = function
      | Node  { value } -> Some value
      | Leaf -> None

    let minimum_key = function
      | Node { key } -> Some key
      | Leaf -> None  

    let remove_minimum heap =
      recurse_with_msg
        ~message:(send ++ "Removing minimum of " +> heap)
        begin match heap with
          | Node { left_subtree; right_subtree } ->
            show_msg (send ++ "Merge the two subtrees.") >>
            merge left_subtree right_subtree >>=
            show_result
          | Leaf -> return Leaf >>= show_result
        end

  end
