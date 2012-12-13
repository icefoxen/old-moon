{
(* moon_lex.mll
   A lexer.  Using ocamllex since I'm lazy.

   Simon Heath
   29/08/2004
*)

exception Lexer_error
exception Eof


let lineNum = ref 1;;
let chrNum = ref 0;;
let fileName = ref "";;


(* Increments line count *)
let nl () =
   lineNum := !lineNum + 1;
   chrNum := 0;;

(* Prints an error message *)
let error msg =
   Printf.eprintf "%s: " !fileName;
   Printf.eprintf "%d.%d: %s\n" !lineNum !chrNum msg;;

(* Resets the error-checker *)
let reset fname =
   fileName := fname;
   lineNum := 1;
   chrNum := 0;;


let fatalError msg = 
   Printf.eprintf "FATAL ERROR: %s: " !fileName;
   Printf.eprintf "%d.%d: %s\n" !lineNum !chrNum msg;
   exit 1;;


let gs = Lexing.lexeme;;

(* Advances the position of the error-checking vars. *)
let adv lb =
  let c = (gs lb) in
(*  if c <> " " then
     Printf.printf "Lexed: '%s'\n" (gs lb);
*)
  chrNum := !chrNum + (String.length (Lexing.lexeme lb));;

(* Chop dangling ".  Also process escapes, if I ever get around to making it
   work.
"*)
let parseStr x = String.sub x 0 (String.length x - 1);;

type token =
    Ident of string
  | String of string
  | Number of float
  | Lparen
  | Rparen
  | Quote
(*  | Colon *)



let print_token = function 
    Ident( s )  -> print_endline ("Ident: " ^ s);
  | String( s ) -> print_endline ("String: " ^ s);
  | Number( f ) -> print_endline ("Number: " ^ (string_of_float f))
  | Lparen      -> print_endline ("Lparen")
  | Rparen      -> print_endline ("Rparen")
  | Quote       -> print_endline ("Quote")


}


let id =
['a'-'z''A'-'Z''_''0'-'9' '.' '/' '-' '+' '*' '%' '!' '@' '#' '$' '^' '&' '<'
'>' '=' '?']* 

let num = '-'?['0'-'9']+ | '-'?['0'-'9']'.'['0'-'9']*



rule start = parse
   '('			{ adv lexbuf; Lparen }
 | ')'			{ adv lexbuf; Rparen }
 | ';'			{ adv lexbuf; lcomment lexbuf }
 | '\n'                 { nl (); start lexbuf }
 | '\r'                 { start lexbuf }
 | ' '                  { adv lexbuf; start lexbuf }
 | '\t'                 { adv lexbuf; start lexbuf }
 | '"'			{ adv lexbuf; grabstr lexbuf }
 | '\''			{ adv lexbuf; Quote }
 (* An idea would be :tbl key: == (getFromTable tbl key).  Hmm. *)
 (*| ":"			{ adv lexbuf; Colon }*)
 | num			{ adv lexbuf; Number( float_of_string (gs lexbuf) ) }
 | id			{ adv lexbuf; Ident( gs lexbuf ) }
 | eof			{ raise Eof }
 | _			{ adv lexbuf; 
                          error ("Invalid token: " ^ (gs lexbuf)); 
			  raise Lexer_error; }

and lcomment = parse
   '\n'			{ nl (); start lexbuf }
 | _			{ adv lexbuf; lcomment lexbuf }


and grabstr = parse
   '"'			{ adv lexbuf; start lexbuf }
 | ("\\\""|[^'"'])*'"'	{ 
   			adv lexbuf; 
			String( parseStr (gs lexbuf) ) }
			       (*(String.sub (gs lexbuf) 
			          0 
				  ((String.length 
				      (gs lexbuf)) - 2) )) } 
				 (Str.first_chars (gs lexbuf) 
				    ((String.length (gs lexbuf)) - 1))) } *)



{
}
