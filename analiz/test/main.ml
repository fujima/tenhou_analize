
open MyStdlib 

let _ =
  let tree_top = Tenhou.parse_in stdin in
    print_endline $ Tenhou.string_of_mjloggm tree_top
