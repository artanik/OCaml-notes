(* ----------------------------------------
   1. Structures
    The structure is usually given a name with the module binding.
    Here is for instance a structure packaging together a type of priority queues and their operations:
   ---------------------------------------- *)

module PrioQueue =
  struct
    type priority = int
    type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
    let empty = Empty
    let rec insert queue prio elt =
      match queue with
          Empty -> Node(prio, elt, Empty, Empty)
        | Node(p, e, left, right) ->
            if prio <= p
            then Node(prio, elt, insert right p e, left)
            else Node(p, e, insert right prio elt, left)
    exception Queue_is_empty
    let rec remove_top = function
        Empty -> raise Queue_is_empty
      | Node(prio, elt, left, Empty) -> left
      | Node(prio, elt, Empty, right) -> right
      | Node(prio, elt, (Node(lprio, lelt, _, _) as left), (Node(rprio, relt, _, _) as right)) ->
          if lprio <= rprio
          then Node(lprio, lelt, remove_top left, right)
          else Node(rprio, relt, left, remove_top right)
    let rec extract = function
        Empty -> raise Queue_is_empty
      | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
  end;;
(*
module PrioQueue :
  sig
    type priority = int
    type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
    val empty : 'a queue
    val insert : 'a queue -> priority -> 'a -> 'a queue
    exception Queue_is_empty
    val remove_top : 'a queue -> 'a queue
    val extract : 'a queue -> priority * 'a * 'a queue
  end
*)

PrioQueue.insert PrioQueue.empty 1 "hello";;
(* - : string PrioQueue.queue = PrioQueue.Node (1, "hello", PrioQueue.Empty, PrioQueue.Empty) *)



(* ----------------------------------------
   2. Signatures - interfaces for structures
   ---------------------------------------- *)

module type PRIOQUEUE =
  sig
    type priority = int (* still concrete *)
    type 'a queue (* now abstract *)
    val empty : 'a queue
    val insert : 'a queue -> int -> 'a -> 'a queue
    val extract : 'a queue -> int * 'a * 'a queue
    exception Queue_is_empty
  end;;
(*
module type PRIOQUEUE =
  sig
    type priority = int
    type 'a queue
    val empty : 'a queue
    val insert : 'a queue -> int -> 'a -> 'a queue
    val extract : 'a queue -> int * 'a * 'a queue
    exception Queue_is_empty
  end
*)

module AbstractPrioQueue = (PrioQueue : PRIOQUEUE);;
(* module AbstractPrioQueue : PRIOQUEUE *)
AbstractPrioQueue.remove_top;;
(* Error: Unbound value AbstractPrioQueue.remove_top *)
AbstractPrioQueue.insert AbstractPrioQueue.empty 1 "hello";;
(* - : string AbstractPrioQueue.queue = <abstr> *)

(* module PrioQueue = (struct ... end : PRIOQUEUE);; *)
(* An alternate syntax is provided for the above: *)
(* module PrioQueue : PRIOQUEUE = struct ... end;; *)



(* ----------------------------------------
   3. Functors - "functions" from structures to structures.
   ---------------------------------------- *)

type comparison = Less | Equal | Greater;;
(* type comparison = Less | Equal | Greater *)

module type ORDERED_TYPE =
  sig
    type t
    val compare: t -> t -> comparison
  end;;
(* module type ORDERED_TYPE = sig type t val compare : t -> t -> comparison end *)

module Set =
  functor (Elt: ORDERED_TYPE) ->
    struct
      type element = Elt.t
      type set = element list
      let empty = []
      let rec add x s =
        match s with
            [] -> [x]
          | hd::tl ->
        match Elt.compare x hd with
            Equal -> s        (* x is already in s *)
          | Less -> x :: s    (* x is smaller than all elements of s *)
          | Greater -> hd :: add x tl
      let rec member x s =
        match s with
            [] -> false
          | hd::tl ->
              match Elt.compare x hd with
                Equal -> true (* x belongs to s *)
              | Less -> false (* x is smaller than all elements of s *)
              | Greater -> member x tl
    end;;
(*
module Set :
  functor (Elt : ORDERED_TYPE) ->
    sig
      type element = Elt.t
      type set = element list
      val empty : 'a list
      val add : Elt.t -> Elt.t list -> Elt.t list
      val member : Elt.t -> Elt.t list -> bool
    end
*)

module OrderedString =
  struct
    type t = string
    let compare x y = if x = y then Equal else if x < y then Less else Greater
  end;;
(* module OrderedString : sig type t = string val compare : 'a -> 'a -> comparison end *)

module StringSet = Set(OrderedString);;
(*
module StringSet :
  sig
    type element = OrderedString.t
    type set = element list
    val empty : 'a list
    val add : OrderedString.t -> OrderedString.t list -> OrderedString.t list
    val member : OrderedString.t -> OrderedString.t list -> bool
  end
*)

StringSet.member "bar" (StringSet.add "foo" StringSet.empty);;
(* - : bool = false *)



(* ----------------------------------------
   4. Functors and type abstraction
   ---------------------------------------- *)

module type SETFUNCTOR =
  functor (Elt: ORDERED_TYPE) ->
    sig
      type element = Elt.t (* concrete *)
      type set (* abstract *)
      val empty : set
      val add : element -> set -> set
      val member : element -> set -> bool
    end;;
(*
module type SETFUNCTOR =
  functor (Elt : ORDERED_TYPE) ->
    sig
      type element = Elt.t
      type set
      val empty : set
      val add : element -> set -> set
      val member : element -> set -> bool
    end
*)

module AbstractSet = (Set : SETFUNCTOR);;
(* module AbstractSet : SETFUNCTOR *)

module AbstractStringSet = AbstractSet(OrderedString);;
(*
module AbstractStringSet :
  sig
    type element = OrderedString.t
    type set = AbstractSet(OrderedString).set
    val empty : set
    val add : element -> set -> set
    val member : element -> set -> bool
  end
*)

AbstractStringSet.add "gee" AbstractStringSet.empty;;
(* - : AbstractStringSet.set = <abstr> *)

module type SET =
  sig
    type element
    type set
    val empty : set
    val add : element -> set -> set
    val member : element -> set -> bool
  end;;
(*
module type SET =
  sig
    type element
    type set
    val empty : set
    val add : element -> set -> set
    val member : element -> set -> bool
  end
*)

module WrongSet = (Set : functor(Elt: ORDERED_TYPE) -> SET);;
(* module WrongSet : functor (Elt : ORDERED_TYPE) -> SET *)

module WrongStringSet = WrongSet(OrderedString);;
(*
module WrongStringSet :
  sig
    type element = WrongSet(OrderedString).element
    type set = WrongSet(OrderedString).set
    val empty : set
    val add : element -> set -> set
    val member : element -> set -> bool
  end
*)

WrongStringSet.add "gee" WrongStringSet.empty;;
(*
Error: This expression has type string but an expression was expected of type
        WrongStringSet.element = WrongSet(OrderedString).element
*)

module AbstractSet2 =
  (Set : functor(Elt: ORDERED_TYPE) -> (SET with type element = Elt.t));;

(*
module AbstractSet2 :
  functor (Elt : ORDERED_TYPE) ->
    sig
      type element = Elt.t
      type set
      val empty : set
      val add : element -> set -> set
      val member : element -> set -> bool
    end
*)

(* As in the case of simple structures, an alternate syntax is provided for dening functors and restricting their result: *)
(* module AbstractSet2(Elt: ORDERED_TYPE) : (SET with type element = Elt.t) = struct ... end;; *)

module NoCaseString =
  struct
    type t = string
    let compare s1 s2 =
    OrderedString.compare (String.lowercase s1) (String.lowercase s2)
  end;;
(*
module NoCaseString :
  sig type t = string val compare : string -> string -> comparison end
*)

module NoCaseStringSet = AbstractSet(NoCaseString);;
(*
module NoCaseStringSet :
  sig
    type element = NoCaseString.t
    type set = AbstractSet(NoCaseString).set
    val empty : set
    val add : element -> set -> set
    val member : element -> set -> bool
  end
*)

NoCaseStringSet.add "FOO" AbstractStringSet.empty;;
(*
Error: This expression has type
        AbstractStringSet.set = AbstractSet(OrderedString).set
        but an expression was expected of type
        NoCaseStringSet.set = AbstractSet(NoCaseString).set
*)



(* ----------------------------------------
   5. Modules and separate compilation
   ---------------------------------------- *)

(* 
 - the implementation file A.ml, which contains a sequence of definitions, analogous to the inside of a struct. . . end construct;
 - the interface file A.mli, which contains a sequence of specifications, analogous to the inside of a sig. . . end construct.

module A: sig (* contents of file A.mli *) end = struct (* contents of file A.ml *) end;;

The files that define the compilation units can be compiled separately using the ocamlc -c
command (the -c option means \compile only, do not try to link"); this produces compiled interface
files (with extension .cmi) and compiled object code files (with extension .cmo). When all units
have been compiled, their .cmo files are linked together using the ocamlc command. For instance,
the following commands compile and link a program composed of two compilation units Aux and
Main:

$ ocamlc -c Aux.mli # produces aux.cmi
$ ocamlc -c Aux.ml # produces aux.cmo
$ ocamlc -c Main.mli # produces main.cmi
$ ocamlc -c Main.ml # produces main.cmo
$ ocamlc -o theprogram Aux.cmo Main.cmo

*)
