let pi = 4.0 *. atan 1.0;;
(* val pi : float = 3.14159265358979312 *)

let square x = x *. x;;
(* val square : float -> float = <fun> *)

let square_int x = x * x;;
(* val square_int : int -> int = <fun> *)

let result_of_squares = square(sin pi) +. square(cos pi);;
(* val result_of_squares : float = 1. *)

let rec fib n =
  if n < 2 then n else fib(n-1) + fib(n-2);;
(* val fib : int -> int = <fun> *)

let result_of_fib = fib 10;;
(* val result_of_fib : int = 55 *)

(* ----------------------------------------
   LIST
   ---------------------------------------- *)

let l = ["is"; "a"; "tale"; "told"; "etc."];;
(* val l : string list = ["is"; "a"; "tale"; "told"; "etc."] *)

"Life" :: l;; (* cons *)
(* - : string list = ["Life"; "is"; "a"; "tale"; "told"; "etc."] *)

let rec sort lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sort tail)
and insert elt lst =
  match lst with
    [] -> [elt]
  | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail
;;
(* val sort : 'a list -> 'a list = <fun> *)
(* val insert : 'a -> 'a list -> 'a list = <fun> *)
(* 'a - an arbitrary type or "all types" *)

sort l;;
(* - : string list = ["a"; "etc."; "is"; "tale"; "told"] *)

sort [6;2;5;3];;
(* - : int list = [2; 3; 5; 6] *)

sort [3.14; 2.718];;
(* - : float list = [2.718; 3.14] *)



(* ----------------------------------------
   Functions as values
   ---------------------------------------- *)

let deriv f dx = function x -> (f(x +. dx) -. f(x)) /. dx;;
(* val deriv : (float -> float) -> float -> float -> float = <fun> *)

let sin' = deriv sin 1e-6;;
(* val sin' : float -> float = <fun> *)

sin' pi;;
(* - : float = -1.00000000013961143 *)

let compose f g = function x -> f(g(x));;
(* val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)

let cos2 = compose square cos;;
(* val cos2 : float -> float = <fun> *)

List.map (function n -> n * 2 + 1) [0;1;2;3;4];;
(* - : int list = [1; 3; 5; 7; 9] *)

let float_list = [0.0;1.0;2.0;3.0;4.0;5.0];;
(* val float_list : float list = [0.; 1.; 2.; 3.; 4.; 5.] *)
List.map square float_list;;
(* - : float list = [0.; 1.; 4.; 9.; 16.; 25.] *)



(* ----------------------------------------
   Records and variants
   ---------------------------------------- *)

type ratio = {num: int; denom: int};;
(* type ratio = { num : int; denom : int; } *)

let add_ratio r1 r2 =
 {num = r1.num * r2.denom + r2.num * r1.denom;
  denom = r1.denom * r2.denom};;
(* val add_ratio : ratio -> ratio -> ratio = <fun> *)

# add_ratio {num=1; denom=3} {num=2; denom=5};;
(* - : ratio = {num = 11; denom = 15} *)

type number = Int of int | Float of float | Error;;
(* type number = Int of int | Float of float | Error *)

type sign = Positive | Negative;;
(* type sign = Positive | Negative *)

let sign_int n = if n >= 0 then Positive else Negative;;
(* val sign_int : int -> sign = <fun> *)

let add_num n1 n2 =
  match (n1, n2) with
    (Int i1, Int i2) ->
      (* Check for overflow of integer addition *)
      if sign_int i1 = sign_int i2 && sign_int(i1 + i2) <> sign_int i1
      then Float(float i1 +. float i2)
      else Int(i1 + i2)
  | (Int i1, Float f2) -> Float(float i1 +. f2)
  | (Float f1, Int i2) -> Float(f1 +. float i2)
  | (Float f1, Float f2) -> Float(f1 +. f2)
  | (Error, _) -> Error
  | (_, Error) -> Error;;
(* val add_num : number -> number -> number = <fun> *)

add_num (Int 123) (Float 3.14159);;
(* - : number = Float 126.14159 *)

(* recursive data structures - example the type of binary trees: *)
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;
(* type 'a btree = Empty | Node of 'a * 'a btree * 'a btree *)

let rec member x btree =
  match btree with
    Empty -> false
  | Node(y, left, right) ->
      if x = y then true else
      if x < y then member x left else member x right;;
(* val member : 'a -> 'a btree -> bool = <fun> *)

let rec insert x btree =
  match btree with
    Empty -> Node(x, Empty, Empty)
  | Node(y, left, right) ->
      if x <= y then Node(y, insert x left, right)
      else Node(y, left, insert x right);;
(* val insert : 'a -> 'a btree -> 'a btree = <fun> *)



(* ----------------------------------------
   Imperative features
   ---------------------------------------- *)

let add_vect v1 v2 =
  let len = min (Array.length v1) (Array.length v2) in
  let res = Array.create len 0.0 in
  for i = 0 to len - 1 do
    res.(i) <- v1.(i) +. v2.(i)
  done;
  res;;
(* val add_vect : float array -> float array -> float array = <fun> *)

add_vect [| 1.0; 2.0 |] [| 3.0; 4.0 |];;
(* - : float array = [|4.; 6.|] *)

type mutable_point = { mutable x: float; mutable y: float };;
(* type mutable_point = { mutable x : float; mutable y : float; } *)

let translate p dx dy =
  p.x <- p.x +. dx; p.y <- p.y +. dy;;
(* val translate : mutable_point -> float -> float -> unit = <fun> *)

let mypoint = { x = 0.0; y = 0.0 };;
(* val mypoint : mutable_point = {x = 0.; y = 0.} *)

translate mypoint 1.0 2.0;;
(* - : unit = () *)

mypoint;;
(* - : mutable_point = {x = 1.; y = 2.} *)

let insertion_sort a =
  for i = 1 to Array.length a - 1 do
    let val_i = a.(i) in
    let j = ref i in
    while !j > 0 && val_i < a.(!j - 1) do
      a.(!j) <- a.(!j - 1);
      j := !j - 1
    done;
  a.(!j) <- val_i
  done;;
(* val insertion_sort : 'a array -> unit = <fun> *)

let current_rand = ref 0;;
(* val current_rand : int ref = {contents = 0} *)
let random () =
  current_rand := !current_rand * 25713 + 1345;
  !current_rand;;
(* val random : unit -> int = <fun> *)

type 'a ref = { mutable contents: 'a };;
(* type 'a ref = { mutable contents : 'a; } *)
let (!) r = r.contents;;
(* val ( ! ) : 'a ref -> 'a = <fun> *)
let (:=) r newval = r.contents <- newval;;
(* val ( := ) : 'a ref -> 'a -> unit = <fun> *)

type idref = { mutable id: 'a. 'a -> 'a };;
(* type idref = { mutable id : 'a. 'a -> 'a; } *)
let r = {id = fun x -> x};;
(* val r : idref = {id = <fun>} *)
let g s = (s.id 1, s.id true);;
(* val g : idref -> int * bool = <fun> *)
r.id <- (fun x -> print_string "called id\n"; x);;
(* - : unit = () *)
g r;;
(* called id *)
(* called id *)
(* - : int * bool = (1, true) *)



(* ----------------------------------------
   Exceptions
   ---------------------------------------- *)

exception Empty_list;;
(* exception Empty_list *)

let head l =
  match l with
    [] -> raise Empty_list
  | hd :: tl -> hd;;
(* val head : 'a list -> 'a = <fun> *)

head [1;2];;
(* - : int = 1 *)

head [];;
(* Exception: Empty_list. *)

List.assoc 1 [(0, "zero"); (1, "one")];;
(* - : string = "one" *)
List.assoc 2 [(0, "zero"); (1, "one")];;
(* Exception: Not_found. *)

let name_of_binary_digit digit =
  try
    List.assoc digit [0, "zero"; 1, "one"]
  with Not_found ->
    "not a binary digit";;
(* val name_of_binary_digit : int -> string = <fun> *)

name_of_binary_digit 0;;
(* - : string = "zero" *)

name_of_binary_digit (-1);;
(* - : string = "not a binary digit" *)



(* ----------------------------------------
   Symbolic processing of expressions
   ---------------------------------------- *)

type expression =
    Const of float
  | Var of string
  | Sum of expression * expression (* e1 + e2 *)
  | Diff of expression * expression (* e1 - e2 *)
  | Prod of expression * expression (* e1 * e2 *)
  | Quot of expression * expression (* e1 / e2 *)
;;
(*
type expression =
    Const of float
  | Var of string
  | Sum of expression * expression
  | Diff of expression * expression
  | Prod of expression * expression
  | Quot of expression * expression
*)

exception Unbound_variable of string;;
(* exception Unbound_variable of string *)

let rec eval env exp =
  match exp with
    Const c -> c
  | Var v ->
      (try List.assoc v env with Not_found -> raise(Unbound_variable v))
  | Sum(f, g) -> eval env f +. eval env g
  | Diff(f, g) -> eval env f -. eval env g
  | Prod(f, g) -> eval env f *. eval env g
  | Quot(f, g) -> eval env f /. eval env g;;
(* val eval : (string * float) list -> expression -> float = <fun> *)

eval [("x", 1.0); ("y", 3.14)] (Prod(Sum(Var "x", Const 2.0), Var "y"));;
(* - : float = 9.42 *)

let rec deriv exp dv =
  match exp with
    Const c -> Const 0.0
  | Var v -> if v = dv then Const 1.0 else Const 0.0
  | Sum(f, g) -> Sum(deriv f dv, deriv g dv)
  | Diff(f, g) -> Diff(deriv f dv, deriv g dv)
  | Prod(f, g) -> Sum(Prod(f, deriv g dv), Prod(deriv f dv, g))
  | Quot(f, g) -> Quot(Diff(Prod(deriv f dv, g), Prod(f, deriv g dv)), Prod(g, g))
;;
(* val deriv : expression -> string -> expression = <fun> *)

deriv (Quot(Const 1.0, Var "x")) "x";;
(* 
- : expression =
Quot (Diff (Prod (Const 0., Var "x"), Prod (Const 1., Const 1.)),
 Prod (Var "x", Var "x"))
*)



(* ----------------------------------------
   Pretty-printing and parsing
   ---------------------------------------- *)

let print_expr exp =
  (* Local function definitions *)
  let open_paren prec op_prec =
    if prec > op_prec then print_string "(" in
  let close_paren prec op_prec =
    if prec > op_prec then print_string ")" in
  let rec print prec exp = (* prec is the current precedence *)
    match exp with
      Const c -> print_float c
    | Var v -> print_string v
    | Sum(f, g) ->
        open_paren prec 0;
        print 0 f; print_string " + "; print 0 g;
        close_paren prec 0
    | Diff(f, g) ->
        open_paren prec 0;
        print 0 f; print_string " - "; print 1 g;
        close_paren prec 0
    | Prod(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " * "; print 2 g;
        close_paren prec 2
    | Quot(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " / "; print 3 g;
        close_paren prec 2
  in print 0 exp;;
(* val print_expr : expression -> unit = <fun> *)

let e = Sum(Prod(Const 2.0, Var "x"), Const 1.0);;
(* val e : expression = Sum (Prod (Const 2., Var "x"), Const 1.) *)
print_expr e; print_newline();;
(* 2. * x + 1. *)
(* - : unit = () *)
print_expr (deriv e "x"); print_newline();;
(* 2. * 1. + 0. * x + 0. *)
(* - : unit = () *)

#load "dynlink.cma";;
#load "camlp4o.cma";;
(* Camlp4 Parsing version 4.03.0 *)

open Genlex;;
let lexer = make_lexer ["("; ")"; "+"; "-"; "*"; "/"];;
(* val lexer : char Stream.t -> Genlex.token Stream.t = <fun> *)

let token_stream = lexer(Stream.of_string "1.0 +x");;
(* val token_stream : Genlex.token Stream.t = <abstr> *)
Stream.next token_stream;;
(* - : Genlex.token = Float 1. *)
Stream.next token_stream;;
(* - : Genlex.token = Kwd "+" *)
Stream.next token_stream;;
(* - : Genlex.token = Ident "x" *)

let rec parse_expr = parser
    [< e1 = parse_mult; e = parse_more_adds e1 >] -> e
  and parse_more_adds e1 = parser
      [< 'Kwd "+"; e2 = parse_mult; e = parse_more_adds (Sum(e1, e2)) >] -> e
    | [< 'Kwd "-"; e2 = parse_mult; e = parse_more_adds (Diff(e1, e2)) >] -> e
    | [< >] -> e1
  and parse_mult = parser
      [< e1 = parse_simple; e = parse_more_mults e1 >] -> e
  and parse_more_mults e1 = parser
      [< 'Kwd "*"; e2 = parse_simple; e = parse_more_mults (Prod(e1, e2)) >] -> e
    | [< 'Kwd "/"; e2 = parse_simple; e = parse_more_mults (Quot(e1, e2)) >] -> e
    | [< >] -> e1
  and parse_simple = parser
      [< 'Ident s >] -> Var s
    | [< 'Int i >] -> Const(float i)
    | [< 'Float f >] -> Const f
    | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e;;

(* val parse_expr : Genlex.token Stream.t -> expression = <fun> *)
(* val parse_more_adds : expression -> Genlex.token Stream.t -> expression = <fun> *)
(* val parse_mult : Genlex.token Stream.t -> expression = <fun> *)
(* val parse_more_mults : expression -> Genlex.token Stream.t -> expression = <fun> *)
(* val parse_simple : Genlex.token Stream.t -> expression = <fun> *)

let read_expression s = parse_expression(lexer(Stream.of_string s));;
(* val read_expression : string -> expression = <fun> *)

read_expression "2*(x+y)";;
(* - : expression = Prod (Const 2., Sum (Var "x", Var "y")) *)
read_expression "x - 1";;
(* - : expression = Diff (Var "x", Const 1.) *)
read_expression "x-1";;
(* Exception: Stream.Error "". *)
