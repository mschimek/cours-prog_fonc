type context = {
  out_chan : out_channel;
  indentation : int;
  switch_on : bool;
}
type effect = context -> context

let init_context out_chan =
  { out_chan; indentation = 0; switch_on = true }

let compose = MorePerv.compose

let no_effect context = context

let apply effect context = effect context 

let indent context = String.make (2 * context.indentation) ' '

let increment context =
  { context with indentation = context.indentation + 1 }

let decrement context =
  { context with indentation = max 0 (context.indentation - 1) }


let msg message context =
  Printf.fprintf context.out_chan "%s%s\n"
    (indent context)
    message;
  context

let switch_on context = { context with switch_on = true }
let switch_off context = { context with switch_on = false }


module AddPrintable = functor (Value : MoreModules.PrintableType) ->
  struct

    let show value context =
      Printf.fprintf context.out_chan "%s%s\n"
        (indent context)
        (Value.to_string value);
      context
    
  end
