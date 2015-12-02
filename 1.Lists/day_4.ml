(* 02/05/2015 *)

#use "day_3.ml";;
(************************************************************************************)
(* 10. Run-length encoding of a list. (easy) *)
(************************************************************************************)

let encode_1 l = let list = pack l in
	       let count = List.map length list in
	       List.map2 (fun a b -> (a,b)) count (compress_sol l)
		 
(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let encode_1_sol list =
	let rec aux count acc = function
	  | [] -> [] (* Can only be reached if original list is empty *)
	  | [x] -> (count+1, x) :: acc
	  | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
	                          else aux 0 ((count+1,a) :: acc) t in
	List.rev (aux 0 [] list);;
(* val encode : 'a list -> (int * 'a) list = <fun> *)

(* An alternative solution, which is shorter but requires 
	more memory, is to use the pack function declared above: *)

let encode_1_sol_1 list =
    List.map (fun l -> (List.length l, List.hd l)) (pack_sol list);;
(* val encode : 'a list -> (int * 'a) list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* 1. High order function !!!!! *)
(* 2. You were right about the counter *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)

		
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 11. Modified run-length encoding. (easy) *)
(************************************************************************************)

type 'a rle =
  | One of 'a
  | Many of (int * 'a)
(* type 'a rle = One of 'a | Many of int * 'a *)
		
let select = function
  | (1, x) ->  One x
  | (_ ,x) as t -> Many t

let encode_2 l = List.map select (encode_1 l)

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let encode_2_sol l =
  let create_tuple cnt elem =
    if cnt = 1 then One elem
    else Many (cnt, elem) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (create_tuple (count+1) x) :: acc
    | hd :: (snd :: _ as tl) ->
        if hd = snd then aux (count + 1) acc tl
        else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
    List.rev (aux 0 [] l);;
(* val encode : 'a list -> 'a rle list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* 1. High order function is short but need more spaces *)
(* 2. Try to write a recursive function *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
