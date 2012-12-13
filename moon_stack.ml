(* moon_stack.ml
   We want to be able to pick and put at arbitrary places in the stack, so
   the standard Ocaml lib doesn't cut the mustard.


   Simon Heath
   30/08/2004
*)


exception Stack_underflow
exception Invalid_index

type 'a t = { mutable stk : 'a array; mutable top : int };;

let createStack x = {
  stk = Array.make 16 x;
  top = 0;
}

(* Doubles the size of the stack *)
let expand s =
  s.stk <- Array.append s.stk (Array.make (Array.length s.stk) s.stk.(0));
  s
;;

let gettop s =
  s.top
;;

let getsize s =
  Array.length s.stk
;;


let push s x = 
  if s.top = ((Array.length s.stk) - 1) then
    ignore (expand s);
  s.stk.(s.top) <- x;
  s.top <- s.top + 1;

;;

let pop s =
  if s.top = 0 then
    raise Stack_underflow
 else (
   s.top <- s.top - 1;
   s.stk.(s.top)
 )
;;

let swap s =
  if s.top < 1 then raise Stack_underflow;
  let x = s.stk.(s.top) in
    s.stk.(s.top) <- s.stk.(s.top - 1);
    s.stk.(s.top - 1) <- x;
;;

(* An index of 0 is the top of the stack, -1 is one from the top, etc.  1 is 
   the bottom of the stack, 2 is the second from the bottom, etc.
*)
let getidx s i =
  if i <= 0 then
    if s.top + i - 1 < 0 then
      raise Stack_underflow
    else
      s.top + i - 1
  else (* i > 0 *)
    if i > s.top then
      raise Invalid_index
    else
      i - 1
;;


let pick s i =
    s.stk.(getidx s i)
;;

let put s i obj =
    s.stk.(getidx s i) <- obj
;;

let exchange s i j =
  let idx1 = getidx s i
  and idx2 = getidx s j in

    let x = s.stk.( idx1 ) in
      s.stk.( idx1 ) <- s.stk.( idx2 );
      s.stk.( idx2 ) <- x
;;

let dropx s x =
  if s.top <= x then
    s.top <- 0
  else
    s.top <- s.top - x
;;

let remove s i =
    for x = (getidx s i) to s.top do
      s.stk.(x) <- s.stk.(x + 1)
    done;

    s.top <- s.top - 1
;;

let insert s i obj =
  let idx = getidx s i in
    for x = s.top downto idx do
      s.stk.(x + 1) <- s.stk.(x)
    done;

    s.top <- s.top + 1;
    s.stk.(idx) <- obj
;; 
