(* 30/03/2015 *)

#use "day_15.ml"

(************************************************************************************)
(* 41. Gray code. (medium) *)
(************************************************************************************)
(* let rec setup = function
  | 0 -> [] 
  | larger -> 0 :: (setup (larger-1))

let rec inc = function
  | [] -> []
  | h::(h1::t) -> if h = 0 then 0::(inc (h1+1)::t) 
                  else :: *)

let prepend c s = 
  


let gray n = 
  let rec aux acc = function 
    | 0 -> acc 
    | larger -> 

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

let prepend c s =
    (* Prepend the char [c] to the string [s]. *)
    let s' = String.create (String.length s + 1) in
    s'.[0] <- c;
    String.blit s 0 s' 1 (String.length s);
    s'
  
  let rec gray_sol n =
    if n <= 1 then ["0"; "1"]
    else let g = gray_sol (n - 1) in
         List.map (prepend '0') g @ List.rev_map (prepend '1') g

(* val prepend : char -> string -> string = <fun> *)
(* val gray : int -> string list = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* notes *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
