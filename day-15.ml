(* 16/03/2020 *)

type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

(* example: And(Or(Var "a", Var "b"), And(Var "a", Var "b")) *)

(************************************************************************************)
(* 39. Truth tables for logical expressions (2 variables). (medium) *)
(************************************************************************************)
(* let table arg1 arg2 rule =  *)
 let rec truth x vx y vy = function
    | Var a -> if a = x then vx 
              else if a = y then vy 
              else  failwith "Invalid expression"
    | Not a -> not (truth x vx y vy a)
    | And (a, b) -> if (truth x vx y vy a) && (truth x vx y vy b)
      then true else false
    | Or (a, b) -> if (truth x vx y vy a) || (truth x vx y vy b)
      then true else false
	
 let table2 x y exp = [(true, true,  truth x true y true),
		       (true, false,  truth x true y false),
		       (false, true,  truth x false y true),
		       (false, false,  truth x false y false)] 
(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let rec eval2 a val_a b val_b = function
    | Var x -> if x = a then val_a
               else if x = b then val_b
               else failwith "The expression contains an invalid variable"
    | Not e -> not(eval2 a val_a b val_b e)
    | And(e1, e2) -> eval2 a val_a b val_b e1 && eval2 a val_a b val_b e2
    | Or(e1, e2) -> eval2 a val_a b val_b e1 || eval2 a val_a b val_b e2

let table2_sol a b expr =
  [(true,  true,  eval2 a true  b true  expr);
   (true,  false, eval2 a true  b false expr);
   (false, true,  eval2 a false b true  expr);
   (false, false, eval2 a false b false expr) ];;
(* val eval2 : string -> bool -> string -> bool -> bool_expr -> bool = <fun> *)
(* val table2 : string -> string -> bool_expr -> (bool * bool * bool) list = <fun>*)
  
(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Sometimes solution IS LONG! *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
    
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 40. Truth tables for logical expressions. (medium) *)
(************************************************************************************)

(* Evaluates the expression with given variables *)
let rec eval vars = function
  | Var a -> List.assoc a vars
  | Not a -> not (eval vars a)
  | And (a, b) -> (eval vars a) && (eval vars b)
  | Or (a, b) -> (eval vars a) || (eval vars b)

(* Make result as desired format *)
let rec help acc vars exp  = match vars with 
  | [] ->  [(List.rev acc, eval vars exp)]
  | h::t -> help ((h, true) :: acc) t exp @ help ((h, false) :: acc) t exp

let table vars exp = help [] vars exp

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

(* [val_vars] is an associative list containing the truth value of
     each variable.  For efficiency, a Map or a Hashtlb should be
     preferred. *)
  
let rec eval val_vars = function
  | Var x -> List.assoc x val_vars
  | Not e -> not(eval val_vars e)
  | And(e1, e2) -> eval val_vars e1 && eval val_vars e2
  | Or(e1, e2) -> eval val_vars e1 || eval val_vars e2

(* Again, this is an easy and short implementation rather than an
   efficient one. *)
let rec table_make val_vars vars expr =
  match vars with
  | [] -> [(List.rev val_vars, eval val_vars expr)]
  | v :: tl ->
     table_make ((v, true) :: val_vars) tl expr
     @ table_make ((v, false) :: val_vars) tl expr

let table_sol vars expr = table_make [] vars expr;;
(* val eval : (string * bool) list -> bool_expr -> bool = <fun>
val table_make :
  (string * bool) list ->
  string list -> bool_expr -> ((string * bool) list * bool) list = <fun>
val table : string list -> bool_expr -> ((string * bool) list * bool) list =
  <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Be farmiliar with high-order functions *)
(* List.assoc can be used as search in dictionary *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)

