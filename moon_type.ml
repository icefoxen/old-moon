(* moon_type.ml
   A hashtable for Moon.
   Right now it just uses Ocaml hashtables.  However, eventually
   it should be rewritten to be most efficient hashing mval keys...
   
   ...Also includes types, since a table is a builtin type.

   Maybe there should also be a way to easily extend types.  Perhaps a
   module or class that can be extended. 

   Simon Heath
   29/08/2004
*)

exception TypeError of string

(* This hashtable should be replaced eventually, with an implementation
   that's really good at hashing strings/symbols.
*)
type tbl = (mval, mval) Hashtbl.t
(* I need this extra level of indirection to make cons cells mutable,
   unfortunately.
*)
and mcons = {mutable mcar : mval; mutable mcdr : mval}
and mval =
    Mstr of string
  | Msymbol of string
  | Mnum of float
  | Mcons of mcons
  | Mtable of tbl
  | Mbuiltin of (vm -> unit) * int
      (* How are functions represented???  As a list, of course. *)
  | Mfunc of mval * int
  | Mnil
  | Mt
and
  (* Virtual machine context, used in Moon_vm *)
  vm = { vms : mval Moon_stack.t; vmt : mval (* MUST be a table! *) }
;;

