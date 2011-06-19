let debug = ref false

let log s =
  if !debug then
    Printf.printf "*** %s\n" s

let logf msgf =
  Printf.kprintf log msgf
