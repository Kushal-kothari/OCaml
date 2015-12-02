(* 09/03/2015 *)

#use "day_14.ml"

(************************************************************************************)
(* 37. Goldbach's conjecture. (medium) *)
(************************************************************************************)

let goldbach n = 
  let rec aux acc x = 
    if (n-acc = x) && is_prime x then (acc, x) 
    else aux (acc+1) (x-1)
  in aux 2 (n-2)


(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

(* [is_prime] is defined in the previous solution *)
  let goldbach_sol n =
    let rec aux d =
      if is_prime d && is_prime (n - d) then (d, n-d)
      else aux (d+1) in
    aux 2
(* val goldbach : int -> int * int = <fun> *)

(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* aux does not need to have second argument, think through before you code *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)
    
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)
(*+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-*)

(************************************************************************************)
(* 38. A list of Goldbach compositions. (medium) *)
(************************************************************************************)

let goldbach_list a b = 
  let rec aux acc d = 
    if d<=b then
      if (d mod 2=0) then aux ((d, goldbach d)::acc) (d+1)
      else aux acc (d+1)
    else acc
  in List.rev (aux [] a)

let goldbach_limit a b c =
  List.filter (fun ( _,(x, y)) ->  x>c && y>c ) (goldbach_list a b)
  

(*==================================================================================*)
(* SOLUTION *)
(*==================================================================================*)

(* [goldbach] is defined in the previous question. *)
  let rec goldbach_list_sol a b =
    if a > b then [] else
      if a mod 2 = 1 then goldbach_list (a+1) b
      else (a, goldbach a) :: goldbach_list (a+2) b
  
  let goldbach_limit_sol a b lim =
    List.filter (fun (_,(a,b)) -> a > lim && b > lim) (goldbach_list a b);;
(* val goldbach_list : int -> int -> (int * (int * int)) list = <fun> *)
(* val goldbach_limit : int -> int -> int -> (int * (int * int)) list = <fun> *)
  
(*==================================================================================*)
(* NOTES *)
(*==================================================================================*)

(* Think about exceptions and use of different high order functions *)

(*==================================================================================*)
(* REVISION *)
(*==================================================================================*)

(* NONE *)

