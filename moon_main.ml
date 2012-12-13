(* moon_main.ml
   Main REP loop.

   Todo: 
   *Make it count parens and keep reading until the final one is closed.
   *Make options for reading files and so on, turning it into a real front-
    end.
   *The string-lexer chokes on comments...
   *Interface with the compiler as well, once you get it.

   Simon Heath
   30/08/2004
*)

open Moon_parser
open Moon_vm

let version = "Moon version 0.1";;

let a () =
  print_endline version;
  let v = createvm () in
  while true do
    print_string "moon> ";
    flush stdout;
    try
      let form = (readString (input_line stdin)) in
      evalList v form;
      Moon_prim.print (pop v);
      print_newline ()
    with
        Vm_exception( x ) -> print_endline ("Error: " ^ x);
      | End_of_file -> (print_endline "\nGoodbye.";
	               exit 1;)
  done;
;;

a ();;
