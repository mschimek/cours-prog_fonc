type interval =
  { left_end : float;
    right_end : float;
  }


let interval left_end right_end = 
  { left_end;
    right_end;
  }

let merge_interval ?(offset = 0.) left right =
  { left_end = min (left.left_end -. offset) (right.left_end +. offset);
    right_end = max (left.right_end -. offset) (right.right_end +. offset);
  }

let add_offset offset interval =
  { left_end = interval.left_end +. offset;
    right_end = interval.right_end +. offset;
  }

let necessary_offset ?(min_gap = 0.) left right =
  max 0. (left.right_end -. right.left_end +. min_gap)


let merge_intervals ?(offset=0.) left_intervals right_intervals =
  MoreList.map2
    ~on_left_remains:(MoreList.map ~f:(add_offset (-. offset /. 2.)))
    ~on_right_remains:(MoreList.map ~f:(add_offset (offset /. 2.)))
    ~f:(merge_interval ~offset:(offset/.2.))
    left_intervals right_intervals

let compute_offset_of_intervals left_intervals right_intervals =
  MoreList.map2
    ~on_left_remains:(fun _ -> [2.])
    ~on_right_remains:(fun _ -> [2.])
    ~f:(necessary_offset ~min_gap:2.)
    left_intervals
    right_intervals
  |> List.fold_left max 0.


let layout_at_node
    (left_intervals,annotated_left_tree)
    (left_tree,root,right_tree)
    (right_intervals, annotated_right_tree) =
  let offset = compute_offset_of_intervals left_intervals right_intervals in
  let new_intervals = merge_intervals ~offset left_intervals right_intervals in
  (interval 0. 0. :: new_intervals,
   BinTree.Node (annotated_left_tree,
                 (root, offset),
                 annotated_right_tree)
  )



let annotate_with_offset bintree =
  BinTree.catamorphism
    ~on_leaf:([],BinTree.Leaf)
    ~on_node:layout_at_node
    bintree


let rec compute_layout_from_offset x_pos y_pos bintree =
  let open BinTree in
  match bintree with
  | Leaf -> Leaf
  | Node (left,(elt,offset),right) ->
    Node (
      compute_layout_from_offset (x_pos -. offset /. 2.) (y_pos -. 1.) left,
      (elt,Gg.P2.v x_pos y_pos),
      compute_layout_from_offset (x_pos +. offset /. 2.) (y_pos -. 1.) right
    )


let compute_box intervals height =
  let proj = List.fold_left merge_interval (interval 0. 0.) intervals in
  Gg.Box2.v
    (Gg.P2.v (proj.left_end -. 1.) (-. float height -. 1.))
    (Gg.Size2.v (proj.right_end -. proj.left_end +. 2.) (float height +. 2.))

let layout bintree =
  bintree
  |> annotate_with_offset
  |> fun (intervals, annotated) ->
  ( compute_box intervals (BinTree.height bintree),
    compute_layout_from_offset 0. 0. annotated
  )


open Vg 
open Gg


let basic_image_of_node (_node_value, node_position) =
  let circle_path =
    P.empty
    |> P.circle P2.o 0.05
  in 
  I.const Color.black
  |> I.cut circle_path
  |> I.move node_position

let basic_image_of_edge (_node_start,start_point) (_node_end,end_point) =
  let area =
    `O { P.o with P.width = 0.02 }
  in 
  let edge_path = 
    P.empty
    |> P.sub start_point
    |> P.line end_point
  in
  I.const Color.black
  |> I.cut ~area edge_path


let image_of_tree
  ?(image_of_node=basic_image_of_node)
  ?(image_of_edge=basic_image_of_edge)
  layout_tree =
  let edges = BinTree.list_of_edges layout_tree in
  let nodes = BinTree.prefix_order layout_tree in
  let draw_edge image (origin,dest) =
    I.blend (image_of_edge origin dest) image
  in
  let draw_node image node =
    I.blend (image_of_node node) image
  in
  I.const Color.white
  |> (fun img -> List.fold_left draw_edge img edges)
  |> (fun img -> List.fold_left draw_node img nodes)

