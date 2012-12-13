(* moon_prim.ml
   Primative operations on very basic things.
   Basically, just handy operations on fundamental types.  These things
   don't even touch the stack; they're used for the runtime and parser
   and such.

   Simon Heath
   30/08/2004
*)

open Moon_type
open Printf


let rec printList x =
  match x with
    Mcons( c ) -> print c.mcar; printList c.mcdr
  | x -> print x; printf ") "


and print x =
  match x with
    Mstr( s ) -> printf "\"%s\" " s
  | Msymbol( s ) -> printf "%s " s
  | Mnum( f ) -> printf "%f " f
  | Mcons( c ) -> printf "("; printList x
  | Mtable( t ) -> printf "#<Table> "
  | Mbuiltin( f, i ) -> printf "#<Builtin: %d> " i
  | Mfunc( f, i ) -> printf "#<Func: %d> " i
  | Mnil -> printf "Nil "
  | Mt -> printf "T "
;;


let rec lst2str x =
  match x with
    Mcons( c ) -> (prim2str c.mcar) ^ (lst2str c.mcdr)
  | x -> (prim2str x) ^ ") "

and prim2str x =
  match x with
    Mstr( s ) -> sprintf "\"%s\" " s
  | Msymbol( s ) -> sprintf "%s " s
  | Mnum( f ) -> sprintf "%f " f
  | Mcons( c ) -> "(" ^ (lst2str x)
  | Mtable( t ) -> sprintf "#<Table> "
  | Mbuiltin( f, i ) -> sprintf "#<Builtin: %d> " i
  | Mfunc( f, i ) -> sprintf "#<Func: %d> " i
  | Mnil -> sprintf "Nil "
  | Mt -> sprintf "T "
;;



let lstcons x y = Mcons( {mcar = x; mcdr = y} );;

let rec list2clist = function
    Mcons( x ) -> x.mcar :: (list2clist x.mcdr)
  | x -> x :: []
;;

let rec clist2list = function
    hd :: tl -> lstcons hd (clist2list tl)
  | [] -> Mnil
;;



let lstappend x y = 
  match x with
      Mcons( x ) -> x.mcdr <- y; Mcons( x )
    | _ -> raise (TypeError "lstappend: Mismatched types!")
;;

let lstcar = function
      Mcons( x ) -> x.mcar
    | _ -> raise (TypeError "lstcar: Mismatched types!")
;;

let lstcdr = function
      Mcons( x ) -> x.mcdr
    | _ -> raise (TypeError "lstcar: Mismatched types!")
;;

let lstsetcar x y =
  match x with
      Mcons( x ) -> (
	x.mcar <- y; 
	Mcons( x ) 
      )
    | _ -> raise (TypeError "lstsetcar: Mismatched types!")
;;

let lstsetcdr x y =
  match x with
      Mcons( x ) -> x.mcdr <- y; Mcons( x )
    | _ -> raise (TypeError "lstsetcdr: Mismatched types!")
;;

let lstlen x =
  let rec loop y i = 
    match y with
	Mcons( c ) -> loop c.mcdr (i + 1)
      | _ -> i
  in
  match x with
      Mcons( _ ) -> loop x 0
    | _ -> raise (TypeError "lstlen: Mismatched types!")
;;


let atomp = function
    Mcons( _ ) -> Mnil
  | Mstr( _ ) -> Mnil
  | _ -> Mt
;;



let lstreverse x =
  let rec lstreversehelper lst accm =
    match (lstcar lst) with
	Mcons( x ) -> lstreversehelper (lstcdr lst) (lstcons (Mcons( x )) accm)
      | x -> (lstcons x accm)
  in
  lstreversehelper (lstcdr x) (lstcons (lstcar x) Mnil)
;;





let tblcreate n = Mtable( Hashtbl.create n );;

let tbladd tbl ky vl =
  match tbl with
      Mtable( t ) ->  Hashtbl.add t ky vl
    | _ -> raise (TypeError "tbladd: Mismatched types!")
;;

let tblget tbl ky = 
  match tbl with
      Mtable( t ) ->  Hashtbl.find t ky 
    | _ -> raise (TypeError "tblget: Mismatched types!")
;;

let tblmem tbl ky = 
  match tbl with
      Mtable( t ) ->  Hashtbl.mem t ky 
    | _ -> raise (TypeError "tblmemr: Mismatched types!")
;;

let tblremove tbl ky = 
  match tbl with
      Mtable( t ) ->  Hashtbl.remove t ky 
    | _ -> raise (TypeError "tblremove: Mismatched types!")
;;

let tblreplace tbl ky newval = 
  match tbl with
      Mtable( t ) ->  Hashtbl.replace t ky newval
    | _ -> raise (TypeError "tblreplace: Mismatched types!")
;;

let tbliter f tbl = 
  match tbl with
      Mtable( t ) ->  Hashtbl.iter f t
    | _ -> raise (TypeError "tbliter: Mismatched types!")
;;
