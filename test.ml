let average a b =
  (a +. b) /. 2.0
(* val average : float -> float -> float = <fun> *)

(* Function with tail recursion *)
let rec range a b =
  if a > b then []
  else a :: range (a + 1) b
(* val range : int -> int -> int list = <fun> *)

(* 'a' and 'b' variables(expressions) is local by let ... in *)
let positive_sum a b = 
  let a = max a 0
  and b = max b 0 in
  a + b
(* val positive_sum : int -> int -> int = <fun> *)

(* ==================================== *)

(* LISTS *)
let languages = ["OCaml"; "Perl"; "C"]
(* val languages : string list = ["OCaml"; "Perl"; "C"] *)

let list_1 = [1;2;3;4;5]
(* al list_1 : int list = [1; 2; 3; 4; 5] *)

let list_2 = 1 :: 2 :: 3 :: 4 :: 5 :: []
(* val list_2 : int list = [1; 2; 3; 4; 5] *)

let list_3 = [1,2,3,4,5] (* tuple in list *)
(* val list_3 : (int * int * int * int * int) list = [(1, 2, 3, 4, 5)] *)

let assoc_list = [(0, "zero"); (1, "one")]
(* val assoc_list : (int * string) list = [(0, "zero"); (1, "one")] *)

(* get value from assoc list by key *)
let value_from_assoc_list = List.assoc 1 assoc_list
(* val value_from_assoc_list : string = "one" *)

(* TUPLES *)
let a_tuple = (3,"three")
(* val a_tuple : int * string = (3, "three") *)

let another_tuple = (3,"four",5.)
(* val another_tuple : int * string * float = (3, "four", 5.) *)

let tuple = 1, 5, 'a', "google"
(* val tuple : int * int * char * string = (1, 5, 'a', "google") *)

(* RECORDS *)
type pair_of_ints = { a : int; b : int }
(* type pair_of_ints = { a : int; b : int; } *)

let some_ints = {a = 3; b = 5}
(* val ints : pair_of_ints = {a = 3; b = 5} *)

type pair_of_float = { a : float; b : float }
(* type pair_of_float = { a : float; b : float; } *)

let some_floats : pair_of_float = {a = 3.; b = 5.}
(* val some_floats : pair_of_float = {a = 3.; b = 5.} *)

(* VARIANTS *)
type foo = Nothing | Int of int | Pair of int * int | String of string
(* type foo = Nothing | Int of int | Pair of int * int | String of string *)

type sign = Positive | Zero | Negative
(* type sign = Positive | Zero | Negative *)

(* RECURCIVE VARIANTS *)
type binary_tree = Leaf of int | Tree of binary_tree * binary_tree
(* type binary_tree = Leaf of int | Tree of binary_tree * binary_tree *)

(* 
Leaf 3
Tree (Leaf 3, Leaf 4)
Tree (Tree (Leaf 3, Leaf 4), Leaf 5)
Tree (Tree (Leaf 3, Leaf 4), Tree (Tree (Leaf 3, Leaf 4), Leaf 5))
*)

(* PARAMETERIZED VARIANTS *)
type 'a binary_tree = Leaf of 'a | Tree of 'a binary_tree * 'a binary_tree
(* type 'a binary_tree = Leaf of 'a | Tree of 'a binary_tree * 'a binary_tree *)

(* PATTERN MATHING *)
type expr = Plus of expr * expr        (* means a + b *)
          | Minus of expr * expr       (* means a - b *)
          | Times of expr * expr       (* means a * b *)
          | Divide of expr * expr      (* means a / b *)
          | Value of string            (* "x", "y", "n", etc. *)

(*
type expr =
    Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Value of string
*)

(* --------------------------------
  The general form for pattern matching

  match object with
    pattern    ->  result
  | pattern    ->  result
      ...
  -------------------------------- *)

let rec to_string e =
  match e with
    Plus (left, right)   -> "(" ^ (to_string left) ^ " + " ^ (to_string right) ^ ")"
  | Minus (left, right)  -> "(" ^ (to_string left) ^ " - " ^ (to_string right) ^ ")"
  | Times (left, right)  -> "(" ^ (to_string left) ^ " * " ^ (to_string right) ^ ")"
  | Divide (left, right) -> "(" ^ (to_string left) ^ " / " ^ (to_string right) ^ ")"
  | Value v -> v
  (* ^ - concatenation operator *)
(* val to_string : expr -> string = <fun> *)

let print_expr e = print_endline (to_string e)
(* val print_expr : expr -> unit = <fun> *)

print_expr (Times (Value "n", Plus (Value "x", Value "y")));;
(* (n * (x + y)) *)

let rec multiply_out e =
  match e with
    Times (e1, Plus (e2, e3)) ->
      Plus (Times (multiply_out e1, multiply_out e2),
            Times (multiply_out e1, multiply_out e3))
  | Times (Plus (e1, e2), e3) ->
      Plus (Times (multiply_out e1, multiply_out e3),
            Times (multiply_out e2, multiply_out e3))
  | Plus (left, right) -> Plus (multiply_out left, multiply_out right)
  | Minus (left, right) -> Minus (multiply_out left, multiply_out right)
  | Times (left, right) -> Times (multiply_out left, multiply_out right)
  | Divide (left, right) -> Divide (multiply_out left, multiply_out right)
  | Value v -> Value v
(* al multiply_out : expr -> expr = <fun> *)

print_expr (multiply_out (Times (Value "n", Plus (Value "x", Value "y"))));;
(* ((n * x) + (n * y)) *)
