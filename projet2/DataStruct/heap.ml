
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

    type alias_t = t
    module Self =
    struct
      type t = alias_t
      let to_string = to_string
    end
    module Format = PrintfFormatter.Make(Self)

    open PrintfEmulation.Computation
    open PrintfEmulation.Computation.Infix
    open Format
    open Format.Formatter


    let recurse computation =
      PrintfEmulation.Computation.nest
        ~before:PrintfContext.increment
        ~after:PrintfContext.decrement
        computation

    
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
      let new_heap = Node {
          left_subtree;
	  key;
          value;
          rank = 1 + rank right_subtree;
	  right_subtree
	}
      in
      msg (send ++ "Gives " +> new_heap)
      >> return new_heap

    let empty = Leaf

    let single key elt = node Leaf key elt Leaf

    let rec merge heap1 heap2 =
      msg (send ++ "Merge " +> heap1 ++ " with " +> heap2) >>
      match heap1, heap2 with
      | Leaf, heap
      | heap, Leaf -> return heap
      | Node ({ key = key1 } as node1), Node { key = key2 } when key1 < key2 ->
        recurse (merge node1.right_subtree heap2)
        >>= node node1.left_subtree key1 node1.value
      | Node node1, Node node2 ->
        recurse (merge heap1 node2.right_subtree)
        >>= node node2.left_subtree node2.key node2.value




    let insert key elt heap =
      msg (send ++ "Insert " ++ Key.to_string key
           ++ "[" ++ Elt.to_string elt ++ "] in "
           +> heap
          ) >> recurse
        ( single key elt
          >>= fun new_heap -> merge new_heap heap
        )

    let minimum = function
      | Node  { value } -> Some value
      | Leaf -> None

    let minimum_key = function
      | Node { key } -> Some key
      | Leaf -> None  

    let remove_minimum heap =
      msg (send ++ "Removing minimum of " +> heap)
      >> match heap with
      | Node { left_subtree; right_subtree } -> merge left_subtree right_subtree
      | Leaf -> return Leaf

  end
