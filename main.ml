open Solveur;;
open Solution;;
open Printf;;

let list_of_int_of_string s =

  
  let t = String.length s
  in
  
  let rec aux s acc i =
    match i with
	0 -> acc
      | _ -> let c = String.sub s 0 1  in
	       match int_of_string_opt c with
		   Some x ->  aux (String.sub s 1 (i-1)) (acc@[x]) (i-1)
		 | None -> aux (String.sub s 1 (i-1)) acc (i-1)
  in
  aux s [] t
;;



let puzzle_of_string s =
  let l = list_of_int_of_string s in
  let rec aux l acc =
    match l with
	[] -> acc
      | c1::c2::imp::t -> aux t acc@[((c1,c2),imp)]
  in
  aux l [];;



let input = Sys.argv.(0) ;;

let p = puzzle_of_string input;;

let sol = Solveur.solve p;;

Solution.print sol;;


