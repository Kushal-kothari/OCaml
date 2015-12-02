(* 02/13/2015 *)

#use "day_9.ml"

(************************************************************************************)
(* 22. Extract a given number of randomly selected elements from a list. (medium) *)
(************************************************************************************)
exception NotFound
  
let rec rand_select ln n = 
  let rec get_nth x = function
    |[] -> raise NotFound
    | h::d -> if x = 0 then h else get_nth (x-1) d
  in 
  let rec random y l = 
    if y = 0 then [] else (Random.int (List.length l))::(random (y-1) l)
  in
  let rec aux acc l = function
    | [] -> acc
    | h::d -> aux (acc @ [get_nth h l]) l d
  in aux [] ln (random n ln) 

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let rec rand_select_sol list n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux n acc list len =
    if n = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (n-1) (picked :: acc) rest (len-1)
  in
  let len = List.length list in
  aux (min n len) [] list len
(* val rand_select : 'a list -> int -> 'a list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Study the solution, not using pattern matching as many as you did *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)

    
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 23. Lotto: Draw N different random numbers from the set 1..M. (easy) *)
(************************************************************************************)

let rec lotto_select n m =
  rand_select (range 1 m) n

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

(* [range] and [rand_select] defined in problems above *)
  let lotto_select_sol n m = rand_select (range 1 m) n
(* val lotto_select : int -> int -> int list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Good Job *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
