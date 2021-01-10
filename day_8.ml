(* 09/02/2020 *)

#use "day_7.ml"

(************************************************************************************)
(* 18. Extract a slice from a list. (medium) *)
(************************************************************************************)

let rec slice list hd tl = 
  let rec aux i acc = function
    | [] -> acc 
    | h::t -> if i < hd then aux (i+1) acc t
      else if i = tl then acc@[h]
      else h::(aux (i+1) acc t)
  in aux 0 [] list

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let slice_sol list i k =
    let rec take n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: take (n-1) t
    in
    let rec drop n = function
      | [] -> []
      | h :: t as l -> if n = 0 then l else drop (n-1) t
    in
    take (k - i + 1) (drop i list);;
(* val slice : 'a list -> int -> int -> 'a list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Nicely done but work on coding style, try to make it easier to understand *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
    
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 19. Rotate a list N places to the left. (medium) *)
(************************************************************************************)

let rec shift_by_left = function
  | [] -> []
  | [h] -> [h]
  | h1::h2::t -> h2::(shift_by_left (h1::t))

let shift_by_right l =
  let rec aux acc n = 
    if n = 1 then acc else  shift_by_right (shift_by_left acc) (n-1) in
  aux l (length l)
  
let rec rotate l n = 
   if n =  0 then l
   else if n < 0 then rotate (shift_by_right l) (n+1)
   else rotate (shift_by_left l) (n-1)

  
(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i-1) (h :: acc) t  in
    aux n [] list
  
let rotate_sol list n =
  let len = List.length list in
  (* Compute a rotation value between 0 and len-1 *)
  let n = if len = 0 then 0 else (n mod len + len) mod len in
  if n = 0 then list
  else let a, b = split list n in b @ a;;

(* val split : 'a list -> int -> 'a list * 'a list = <fun> *)
(* val rotate : 'a list -> int -> 'a list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* notes *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)



