(*---------------------------------------------------------------------------------------------------------------------*)

(*#use "Session4@080425.ml";;*)

(*Covers Lecture 4: More on Lists*)
(*Focus on take and drop*)

(*---------------------------------------------------------------------------------------------------------------------*)

(* zip [x1 ; x2 ; x3] [y1 ; y2 ; y3] = [(x1 , y1)  (x2 , y2) ; (x3, y3)]*)

exception UnequalLists;;

let rec zip (list1 : 'a list) (list2 : 'b list) : ('a * 'b) list = 
  match list1, list2 with 
  | [], [] -> []
  | (x :: xs), [] -> raise UnequalLists
  | [], (y :: ys) -> raise UnequalLists
  | x::xs, y::ys -> (x, y) :: zip(xs) (ys)

(* zip (xs) (ys) *)

(*
let xs = [1 ; 2 ; 3 ; 4];;

let ys = ["a" ; "b" ; "c" ; "d"];;
*)

(* unzip [(x1 , y1)  (x2 , y2) ; (x3, y3)] = ([x1 ; x2 ; x3], [y1 ; y2 ; y3])*)

let rec unzip (list : ('a * 'b) list) : ('a list) * ('b list) = 
  match list with
  (*base case*)
  | [] -> ([], [])
  (*both lines are equivalent in ocaml *) 
 (* | (a, b)::xs -> let (alist, blist) = unzip xs in (a::alist, b :: blist) *)
  | (a, b)::xs -> match (unzip xs) with 
  | (alist, blist) -> (a :: alist, b :: blist)

(*
let x = 1 in x + 3 ---> 1 + 3
let y = 4 in f(y) ---> f(4)
let ... =  .... in ... that just creates a name binding

a = 1
b = "a"
xs = [(2, "b"); (3, "c"); (4, "d")]
*)

(*
unzip [(1, "a"); (2, "b"); (3, "c"); (4, "d")]
= unzip((1, "a")) ::  [ (2, "b"); (3, "c"); (4, "d")])
= let (alist, blist) = unzip [ (2, "b"); (3, "c"); (4, "d")]  
    in  (1 :: alist, "a" :: blist)
= let (alist, blist) = ([2 ; 3 ; 4], ["b" ; "c" ; "d"]) 
    in  (1 :: alist, "a" :: blist)
= let alist = [2 ; 3 ; 4] in let blist = ["b" ; "c" ; "d"] 
    in  (1 :: alist, "a" :: blist)
= ([1 ; 2 ; 3 ; 4], ["a" ; "b" ; "c" ; "d"])
*)

(*
the second version: 
unzip [(1, "a"); (2, "b"); (3, "c"); (4, "d")]
= unzip((1, "a")) ::  [ (2, "b"); (3, "c"); (4, "d")])
= match (unzip [(2, "b"); (3, "c"); (4, "d")]) with 
    | (alist, blist) -> (1 :: alist, "a" :: blist)
(*recursive call is unzip [(2, "b"); (3, "c"); (4, "d")] = ([2 ; 3 ; 4], ["b" ; "c" ; "d"]) *)
= match ([2 ; 3 ; 4], ["b" ; "c" ; "d"])  with 
    | (alist, blist) -> (1 :: alist, "a" :: blist)
= (1 :: [2 ; 3 ; 4], "a" :: ["b" ; "c" ; "d"])
= ([1 ; 2 ; 3 ; 4], ["a" ; "b" ; "c" ; "d"])
*)

(*
unzip (zip p) = p
let (a, b) = unzip xs in zip a b = xs
*)

(*---------------------------------------------------------------------------------------------------------------------*)
