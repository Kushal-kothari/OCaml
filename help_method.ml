
(* print a list of string *)
let rec print_list  = function
  |[] -> ()
  |h::t -> print_string h; print_list t


let rec extract x acc= function
    | [] -> raise NotFound
    | h::d -> if x = 0 then (h,acc@d) else extract (x-1) d
