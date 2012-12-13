(* moon_vm.ml
   Virtual machine.  A stack and a symbol table.  Functions to fiddle with 
   them.  Preeetty straightforward.

   Fundamental operations:
   Push, pop, assign or find value in table, funcall/eval, callprim, 
   conditional/unconditional jump (???), move, remove, swap, 

   Primative functions:
   cons, car, cdr, setcar, setcdr, add, sub, mul, div, mod, if, cond,
   exit, append...

   Local vars are just let on the stack, at the moment.
   Calling conventions are like lua.  Everything is on the stack, most
   functions take an index as a reference to their argument(s)

   Hrm.  For a plain Scheme-y language, the stack is a bit redundant, even
   wasteful.
   But if we compile to bytecode, then it's important!
   Hmmm.  Bytecode also makes it easier to represent functions, sorta...
   They're just a bunch of instructions with an address.

   Simon Heath
   30/08/2004
*)

open Moon_type
open Moon_prim

exception Vm_exception of string


let createvm () =
  { vms = Moon_stack.createStack Mnil;
    vmt = tblcreate 64;
  }
;;
  
let pushconst vm itm =
  Moon_stack.push vm.vms itm
;;


(* These aren't strictly necessary...
(* Moves an item from in the stack to the top *)
let lift vm i =
  let n = Moon_stack.pick vm.vms i in
    Moon_stack.remove vm.vms i;
    Moon_stack.push vm.vms n
;;

(* Copies an item from in the stack to the top *)
let copylift vm itm =
  let n = Moon_stack.pick vm.vms i in
    Moon_stack.push vm.vms n
;;

let callprim vm ky =
  if not (tblmem vm.vmt ky) then
    raise (Vm_exception "Var does not exist!")
  else
    match (tblget vm.vmt ky) with
	Mbuiltin( f, i ) -> f vm
      | x -> raise (Vm_exception ((prim2str x) ^ " is not a function!"))

;;

*)
  

let pop vm =
  Moon_stack.pop vm.vms
;;

let remove vm idx =
  Moon_stack.remove vm.vms idx
;;

(* Takes the top two items and uses the lower one as a key, the upper as a
   value, and binds them in the global table.  *)
let varbind vm =
  let vl = Moon_stack.pop vm.vms in
  let ky = Moon_stack.pop vm.vms in
    tbladd vm.vmt ky vl
;;

(* Gets an object from the table; idx is the key *)
let varget vm idx =
  try
    Moon_stack.push vm.vms (tblget vm.vmt (Moon_stack.pick vm.vms idx))
  with
      Not_found -> raise (Vm_exception "Var does not exist!")
;;

let varrem vm =
  let key = Moon_stack.pop vm.vms in
    tblremove vm.vmt key
;;

let varexists vm =  
  let key = Moon_stack.pop vm.vms in
    tblmem vm.vmt key
;;

(* Resolves and pushes a var *)
let pushvar vm obj =
  if not (tblmem vm.vmt obj) then
    raise (Vm_exception ("Var " ^ (prim2str obj) ^ " does not exist!"))
  else
    Moon_stack.push vm.vms (tblget vm.vmt obj)
;;

  


(* Binds an object to a table on the stack, idx is the index of the table *)
let stblbind vm idx =
  let tbl = Moon_stack.pick vm.vms idx in
  let vl = Moon_stack.pop vm.vms in
  let ky = Moon_stack.pop vm.vms in
    tbladd tbl ky vl
;;

(* Gets an object from the table pointed to by idx.  
   Key is the first item on the stack.*)
let stblget vm idx =
  let t = Moon_stack.pick vm.vms idx in
  let ky = Moon_stack.pop vm.vms in
    tblget t ky
;;

let stblrem vm idx =
  let t = Moon_stack.pick vm.vms idx in
  let ky = Moon_stack.pop vm.vms in
    tblremove t ky
;;


let stblexists vm idx =
  let t = Moon_stack.pick vm.vms idx in
  let ky = Moon_stack.pop vm.vms in
    tblmem t ky
;;


(* Okay, so here's the real monkey.  It takes a Moon value, evaluates it,
   and returns the result on the stack.  And there are a bunch of other
   funcs to handle special forms, as well.
   *If it's a quoted value, a string, or a number, just push it.
   *If it's not, grab it from the hashtable, THEN push it.
   *If it's a list (ie a funcall), eval all the arguments, then...
    well, a function can either be a Ocaml function, in which it's just
    called.  Or it can be a Moon function, which is just another list,
    in which case I suppose it's eval'ed.

   XXX: How do we make sure the right number of args are given to a function?
   Can we do it here?
   Y'see, if this currently just pushes blindly whatever args it's given, 
   so...  A function has to know how many args it takes, and eval has to 
   check that there's the right number before pushing them.  Hmm.  Doable.
*)

let doIf vm body =
  ()
;;

let doLet vm body =
  ()
;;

(* Remember that you always have to return a value on the stack! *)
let doDefine vm =
  let x = (Moon_stack.pick vm.vms (-1)) in
  match x with
      Msymbol( _ ) -> varbind vm; pushconst vm x;
    | _ -> raise (Vm_exception "define: var name must be a symbol!")
;;
    

(*
let doDefine vm body =
  let name = ref Mnil
  and bod = ref Mnil in

  let nm = lstcar body
  and rst = lstcar (lstcdr body) in
    (match nm with
	 Msymbol( _ ) -> bod := nm
       | _ -> raise (Vm_exception "define: var name must be a symbol!"));
    bod := rst;
    
    tbladd vm.vmt !name !bod;

    pushconst vm !name;
;;
*)

let doFunc vm body =
  ()
;;

let doSet vm body =
  ()
;;

let doPrint vm body =
  match body with
   | Mcons( x ) -> Moon_prim.print x.mcar; 
                   Printf.printf "\n"; 
		   pushconst vm Mnil;
   | _ -> raise (Vm_exception "doPrint: must be passed a proper list")
;;

let rec doBegin vm body =
   match body with
      Mcons( x ) -> eval vm x.mcar; doBegin vm x.mcdr;
    | x -> eval vm x

and evalForm vm c =
  (* Leetle helper function here. *)
  let rec evalAndPush car cdr =
    eval vm car;
    match cdr with
	Mcons( x ) -> evalAndPush x.mcar x.mcdr
      | x -> raise (Vm_exception "evalForm: Improper list in funcall!")
  in

  match c.mcar with 
      Msymbol( x ) ->
	(* If it's a symbol, we first find out what kind of function it is.
	   We need to do this 'cause certain things DON'T evaluate their args.
	   Special forms, y'see.  So far they are:
	   quote, let, define, begin, func (lambda), if, set 
	   Ones I need: for, foreach, cond *)
	if x = "quote" then
	  match c.mcdr with
	      Mcons( x ) -> pushconst vm x.mcar
	    | _ -> raise (Vm_exception "eval: Type mismatch in quote")
	else if x = "let" then
	  doLet vm c.mcdr
	else if x = "if" then
	  doIf vm c.mcdr
	else if x = "begin" then
	  doBegin vm c.mcdr
	else if x = "func" then
	  doFunc vm c.mcdr
	else if x = "set" then
	  doSet vm c.mcdr
	else if x = "print" then 
          doPrint vm c.mcdr
	else if x = "define" then
	  if (lstlen c.mcdr) = 2 then (
	    pushconst vm (lstcar c.mcdr);
	    pushconst vm (lstcar (lstcdr c.mcdr));
	    doDefine vm;
	  )
	  else
	    raise (Vm_exception 
		     (Printf.sprintf 
			"define expects 2 args, got %d" (lstlen c.mcdr)))

	else
	  (* Get the symbol's value and try again *)
	  (try
	    evalForm vm {mcar = (tblget vm.vmt c.mcar); mcdr = c.mcdr}
	  with
	      Not_found -> raise 
		(Vm_exception ("Function " ^ (prim2str c.mcar) ^ 
		              " does not exist!")))
    | Mfunc( x, i ) ->
	(* If it's a defined function, we evaluate the args, push 'em, then
	   evaluate the function body. *)
	if (lstlen c.mcdr) = i then (
	  match c.mcdr with 
	      Mcons( d ) -> evalAndPush d.mcar d.mcdr;
	    | x -> eval vm x;
	  eval vm c.mcar;
	)
	else
	  raise (Vm_exception 
		   (Printf.sprintf 
		      "Function expects %d args, got %d" i (lstlen c.mcdr)))
    | Mbuiltin( x, i ) ->
	(* If it's a built-in function, we evaluate the args, push 'em, then
	   call the thing. *)
	if (lstlen c.mcdr) = i then (
	  (match c.mcdr with 
	      Mcons( d ) -> evalAndPush d.mcar d.mcdr
	    | y -> eval vm y);
	  x vm;
	)
	else
	  raise (Vm_exception 
		   (Printf.sprintf 
		      "Function expects %d args, got %d" i (lstlen c.mcdr)))
    | Mcons( x ) ->
	(* If it's a list, we evaluate it, assuming it's gonna return a 
	   function, then try again with the result. *)
	evalForm vm x;
	evalForm vm {mcar = pop vm; mcdr = c.mcar}
    | x -> 
	(* Otherwise, it's no good. *)
	raise (Vm_exception ((prim2str x) ^ " is not a function!"))

and eval vm vl =
  match vl with
      Mcons( c )     -> evalForm vm c
    | Msymbol( _ )   -> pushvar vm vl
    | Mnum( _ )      -> pushconst vm vl
    | Mstr( _ )      -> pushconst vm vl
    | Mtable( _ )    -> pushconst vm vl
    | Mfunc( _ )     -> pushconst vm vl
    | Mbuiltin( _ )  -> pushconst vm vl
    | Mt             -> pushconst vm vl
    | Mnil           -> pushconst vm vl
;;

(* Evaluates an OCAML list of statements, as returned by the parser. *)
let evalList vm vlst =
  List.iter (fun x -> eval vm x) vlst
;;
