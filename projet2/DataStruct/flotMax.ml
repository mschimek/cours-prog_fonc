type annotation = Arc of (int*int)  (* flot*capacity  *)


type direction =
  | Forward of (int*int)
  | Backward of (int*int)
  
 

module FlotMax = 
  struct 
   
    type anchestor =
      | Father of (int)
      | Root
    
	
    let printList list n = 
      Printf.printf "%d :   " n;
      List.iter (fun elem -> Printf.printf "%d," elem) list;
      Printf.printf "\n";;

    let print_path list =
       List.iter (fun (src,dest) -> 
		  Printf.printf "Src:%d, Dest:%d; " src dest)
		 list;
       Printf.printf "\n";;

    let printVSet set n =
      Printf.printf "%d :   " n;
      Graph.VSet.iter (fun elem -> Printf.printf "%d," elem) set;
      Printf.printf "\n";;

    let print_helper key ancestor =
      match ancestor with
      |Root -> Printf.printf "Vertice: %d Root;  " key
      |Father(elem) -> Printf.printf "Vertice: %d Father: %d;  " key elem

    let printVMap map =
      Graph.VMap.iter 
	(fun key elem -> print_helper key elem)
	map;
      Printf.printf "\n";;

    let augmenting_path ~t map =
      let rec accu cur list =
	match Graph.VMap.find cur map with
	| None -> []
        | Some(Root) -> list
        | Some(Father(elem)) -> accu elem  ((elem,cur)::list)
      in
      accu t []
	   

    let add_direction info path =
      let forward (src,dest) = 
	match Graph.EMap.find (src,dest) info with 
	| None -> false
        | Some(flot,cap) -> cap - flot > 0 in
      let value src dest = Graph.EMap.unsafe_find (src,dest) info
      in
      List.fold_left (fun pathMap (src,dest) -> 
		      if forward (src,dest) then 
			Graph.EMap.add 
			  (src,dest) 
			  (Forward((value src dest)))
			  pathMap

		      else
			Graph.EMap.add 
			  (dest,src) 
			  (Backward((value dest src)))
			  pathMap
		     )
		     Graph.EMap.empty
		     path
			
				      
     let exchange_value_if_condition cond ex value =
       if (cond value) then ex 
       else value
		     
     let find_max path_map =
       Graph.EMap.fold (fun key elem max -> 
		   match elem with 
		   | Forward(flot,cap) when cap-flot < max -> cap-flot
                   | Backward(flot,cap) when flot < max -> flot
		   | _ -> max
		  )
		  path_map
		  max_int
       |> exchange_value_if_condition (fun elem -> elem = max_int) 0 
	      

     let printEMap map = 
       Graph.EMap.iter
	 (fun (src,dest) elem  -> 
	  match elem with
	  | Forward (flow,cap) -> 
	     Printf.printf "Forward: src: %d dest: %d  flow: %d cap: %d\n" 
			   src dest flow cap
	  | Backward (flow,cap) ->
	      Printf.printf "Backward: src: %d dest: %d  flow: %d cap: %d\n" 
			   src dest flow cap)
	 map 

     let printSimpleEMap map = 
       Graph.EMap.iter
	 (fun (src,dest) (flow,cap) -> 
	  Printf.printf "src: %d dest: %d  flow: %d cap: %d\n" src dest flow cap)
	 map


     let update_flow info path_map flow_delta =
       let fold = 
	 (fun key elem accu_info ->
	 match elem with
	 | Forward(flot,cap) -> 
	    Graph.EMap.add key (flot+flow_delta,cap) accu_info
	 | Backward(flot,cap) -> 
	    Graph.EMap.add key (flot-flow_delta,cap) accu_info )
       in
       Printf.printf "\n path_map \n";
       printEMap path_map;
       Printf.printf "info \n";
       Graph.EMap.fold fold
		       path_map
		       info
      
    
    let filter_outgoing arcs info =
      Graph.ESet.filter (fun elem -> 
			 match Graph.EMap.find elem info with
			 | None -> failwith "filter_outgoing : arc not found"
			 | Some(flot,capacity) -> (capacity - flot > 0))
			arcs
     		
    let filter_incoming arcs info =
      Graph.ESet.filter (fun elem ->
			 match Graph.EMap.find elem info with 
			 | None -> failwith "filter_incoming : arc not found"
			 | Some(flot, capacity) -> flot > 0 )
			arcs
				       
    let delta_out_in graph vertex info = 
      let delta_out = filter_outgoing (Graph.delta_out vertex graph) info in
      let delta_in = filter_incoming (Graph.delta_in vertex graph) info in 
      Graph.ESet.fold (fun elem set -> Graph.ESet.add elem set) 
		      delta_in
		      delta_out
      
    
    let not_already_reached reached arcs vertex =
      let not_reached = 
	Graph.ESet.filter
	  (fun arc -> 
	   match arc with 
	   | (src,dest) when src = vertex
	     -> Pervasives.not (Graph.VSet.mem dest reached)
	   | (src,dest) when dest =vertex  -> 
	      Pervasives.not (Graph.VSet.mem src reached)
           | _ -> false)
	  arcs
      in
      Graph.ESet.fold 
	(fun  (src,dest) accu -> 
	  match (src,dest) with 
	   | (src,dest) when src = vertex -> 
	      Graph.VSet.add dest accu
	   | (src,dest) when dest =vertex  -> 
	      Graph.VSet.add src accu 
	   | _ -> accu)
	not_reached 
	Graph.VSet.empty
	       

    let rec bfs_loop graph info reached frontier father n target =
      let open MoreQueue in
      match MoreQueue.observe_left frontier with 
      | None -> father
      | Some(cur_vertex,new_queue) ->
	 (*Printf.printf "n: %d  newVertex : %d\n" n cur_vertex; *)
	 let out = delta_out_in graph cur_vertex info in
         let out_not_reached = not_already_reached reached out cur_vertex in
	 let new_frontier = Graph.VSet.fold
			      (fun elem accu -> MoreQueue.add_right accu elem)
			      out_not_reached
			      new_queue in
	 
	 let new_reached = Graph.VSet.fold
			     (fun elem accu -> Graph.VSet.add elem accu)
			     out_not_reached
			     reached in
	 let new_father = Graph.VSet.fold
			    (fun elem father -> 
			     Graph.VMap.add elem (Father(cur_vertex)) father)
			    out_not_reached
			    father
	 in
	 (*printVSet out_not_reached n; *)
	 (*Printf.printf "---------";
	 printVMap new_father;*)
	 if Graph.VSet.mem target out_not_reached  then
	   new_father
	 else
	   bfs_loop graph info new_reached new_frontier new_father (n+1) target
	  
		  
		  
    let bfs graph info start target =
      let reached = Graph.VSet.add start Graph.VSet.empty in
      let frontier = MoreQueue.add_left start MoreQueue.empty in
      let father = Graph.VMap.add start Root  Graph.VMap.empty
      in
      bfs_loop graph info reached frontier father 1 target
	       

    let rec edmonds_karp graph  ~start ~target  info =
      let augmenting_path = (bfs graph info start target)
			    |> augmenting_path ~t:target  in
      let path_map = add_direction info augmenting_path in 
      let flow_delta = find_max path_map in
      let new_info = update_flow info path_map flow_delta
      in
      printSimpleEMap new_info;
      Printf.printf "flow: %d  " flow_delta; 
      print_path augmenting_path;
      match augmenting_path with
      | [] -> (graph,info) 
      | _ ->  edmonds_karp graph ~start ~target new_info
   
  end
      





let graph = Graph.add_vertex 0 Graph.empty 
let graph = Graph.add_vertex 1 graph
let graph = Graph.add_vertex 2 graph
let graph = Graph.add_vertex 3 graph
let graph = Graph.add_arc ~src:0 ~dst:1 graph

let graph = Graph.add_arc ~src:0 ~dst:2 graph

let graph = Graph.add_arc ~src:1 ~dst:2 graph

let graph = Graph.add_arc ~src:1 ~dst:3 graph

let graph = Graph.add_arc ~src:2 ~dst:3 graph



let info = Graph.EMap.add (0,1) (0,4) Graph.EMap.empty
let info = Graph.EMap.add (0,2) (0,2) info

let info = Graph.EMap.add (1,2) (0,3) info

let info = Graph.EMap.add (1,3) (0,1) info

let info = Graph.EMap.add (2,3) (0,6) info

let printSimpleEMap map = 
  Graph.EMap.iter
    (fun (src,dest) (flow,cap) -> 
     Printf.printf "src: %d dest: %d  flow: %d cap: %d\n" src dest flow cap)
    map

(*let res = FlotMax.bfs graph info 0 3 
let _  = Printf.printf "res\n"; (FlotMax.printVMap res) *)

let (graph, map_with_flow) = FlotMax.edmonds_karp graph ~start:0 ~target:3 info 

let _ = 
  Printf.printf " \n\nresultat \n";
  printSimpleEMap map_with_flow

(*
let path = FlotMax.augmenting_path  3 res
let e = if path = [] then Printf.printf "empty path\n"
 else Printf.printf"full path\n"
let _ = FlotMax.print_path path *)

(*let _ = FlotMax.edmonds_karp graph  ~start:0 ~target:3 info*)




  
 

    
        
