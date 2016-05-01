let cons elt list = elt :: list

let rec repeat ~f n init =
  if n < 0 then init
  else repeat ~f (n-1) (f init)


let compose f g x = f (g x)



type 'value comparison = 'value -> 'value -> int
  
let compose_compare compare1 compare2 (x1,y1) (x2,y2) =
  let delta = compare1 x1 x2 in
  if delta <> 0 then delta
  else compare2 y1 y2




  
