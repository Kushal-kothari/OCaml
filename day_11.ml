(* 16/02/2020 *)

#use "day_10.ml"

(************************************************************************************)
(* 24. Generate a random permutation of the elements of a list. (easy) *)
(************************************************************************************)

let rec permutation ln =
  let rec extract x acc = function
    | [] -> raise NotFound
    | h::d-> if x = 0 then (h, acc@d) else extract (x-1) (h::acc) d
  in
  let rec aux acc = function
    | [] -> acc
    | l -> let (a, b) = (extract (Random.int (List.length l)) [] l) in aux (acc@[a]) b
  in aux [] ln

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let rec permutation list =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux acc list len =
    if len = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (picked :: acc) rest (len-1)
  in
  aux [] list (List.length list);;
(* val permutation : 'a list -> 'a list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Same idea as solution *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
    


