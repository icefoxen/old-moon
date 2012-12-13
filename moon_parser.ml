(* moon_parser.ml
   Parser for Moonshine.
   Hmm.
   NOT thread-safe!

   Simon Heath
   29/08/2004
*)

open Moon_lex
open Moon_type
open Moon_prim

exception Parser_error of string

let lexbuf = ref (Lexing.from_channel stdin);;


let rec parse = function
   String( s ) -> Mstr( s )
   (* A slight hack to make () == Nil *)
 | Lparen -> 
      let x = (start !lexbuf) in
         (match x with 
	    Rparen -> Mnil 
	  | x -> parseLst x)
 | Rparen -> raise (Parser_error "Unexpected ) found")
 | Number( f ) -> Mnum( f )
 | Ident( s ) -> 
      if s = "Nil" then Mnil 
      else if s = "T" then Mt 
      else Msymbol( s )
 | Quote -> parseQuote ()


(* For efficiency, we cons things to the front of a list, and reverse it when
   we return *)
and parseLst itm = 
   let rec loop n = 
      let x = start !lexbuf in
      match x with 
         Rparen -> List.rev n
       | x -> loop ((parse x) :: n)

   in
      clist2list (loop [(parse itm)])


(* Turn 'foo into (quote foo) *)
and parseQuote () =
   lstcons (Msymbol( "quote" )) (lstcons (parse (start !lexbuf)) Mnil)
;;

let readStream s =
   let x = ref [] in
   lexbuf := Lexing.from_channel s;
   (* Using an exception as a control structure ain't great, but... *)
   (try
      while true do
         x := (parse (start !lexbuf)) :: !x
      done;
   with
      Eof -> ()); (* Printf.printf "Lexing successful!  Yay!\n"; *)
   x := List.rev !x;

   !x;   
;;

let readString s =
  let x = ref [] in
    lexbuf := Lexing.from_string s;
    (* Using an exception as a control structure ain't great, but... *)
    (try
      while true do
        x := (parse (start !lexbuf)) :: !x
      done
     with
	Eof ->  ()); (* Printf.printf "Lexing successful!  Yay!\n"; *)
      
      x := List.rev !x;
    
    (* Print everything out *)
    List.iter print !x;
    print_newline ();
    !x;
;;


let openFile fn =
   reset fn;
   try
      open_in fn;
   with
      Sys_error a -> fatalError "File does not exist: %s\n" a
;;

let readFile fn =
   let x = (openFile fn) in
   let a = readStream x in
   close_in x;
   a
;;

