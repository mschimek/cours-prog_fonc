module Int = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end

module Str = struct
  type t = string
  let compare = String.compare
  let to_string str = str
end


module IHeap = Heap.Make(Int)(Str)


let item_list =
  MoreList.range 0 5
  |> List.map (fun i -> char_of_int (i + int_of_char 'a'))
  |> List.map (String.make 2)
  |> MoreList.index
  |> MoreList.shuffle

open PrintfEmulation.Computation.Infix

let all_insertion =
  List.fold_left
    (fun heap (int,string) -> heap >>= IHeap.insert int string)
    (PrintfEmulation.Computation.return IHeap.empty)
    item_list


let final_heap =
  PrintfEmulation.Emulator.execute
    all_insertion
    (PrintfContext.init_context stdout)
