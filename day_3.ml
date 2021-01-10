(* 04/02/2020 *)

#use "day_2.ml";;
(************************************************************************************)
(* 7. Flatten a nested list structure. (medium) *)
(************************************************************************************)

(* There is no nested list type in OCaml, so we need to define one first. 
  A node of a nested list is either an element, or a list of nodes. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list
(* type 'a node = One of 'a | Many of 'a node list *)

let rec flatten = function
  | [] -> []
  | (One x)::tl -> x::(flatten tl)
  | (Many x)::tl -> (flatten x)@(flatten tl)

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

(* This function traverses the list, prepending any encountered elements
    to an accumulator, which flattens the list in inverse order. It can
    then be reversed to obtain the actual flattened list. *)
  
let flatten_sol list =
  let rec aux acc = function
		| [] -> acc
		| One x :: t -> aux (x :: acc) t
		| Many l :: t -> aux (aux acc l) t in
List.rev (aux [] list)

(* val flatten : 'a node list -> 'a list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Again, tail recursion, but I think my solution is simpler *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

let flatten_rev l =
  let rec help acc = function
     | [] -> acc
     | One x :: t -> help (acc @ [x]) t
     | Many x :: t -> help (help acc x ) t
  in help [] l
		
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
(************************************************************************************)

let rec compress = function
  | [] -> [] 
  | [x]-> [x]
  | h1::h2::t -> if h1 = h2 then compress (h1::t) else h1::(compress (h2::t))

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let rec compress_sol = function
    | a :: (b :: _ as t) -> if a = b then compress_sol t else a :: compress_sol t
    | smaller -> smaller
    (* val compress : 'a list -> 'a list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* 1. Use keywords such as "as" to make your function more elegant and efficient *)
(* 2. Again, coding style is VERY IMPORTANT, a good programmer code cleanly and elegantly *)
(* 3. Question with "smaller" *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
		
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)
(************************************************************************************)

let rec pack = function
	| a :: (b :: _ as t ) -> if a <> b then [a]::pack t else [a; b] :: pack t
	| smaller -> [smaller]

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let pack_sol list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list)
(* val pack : 'a list -> 'a list list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* notes *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)



