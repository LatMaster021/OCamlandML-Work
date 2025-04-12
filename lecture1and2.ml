(*#use 'lecture4and5.ml';;*)

(*Covers zip and unzip from lecture 4 and sorting from lecture 5 *)

(*Zip adn Unzip*)


(* zip [x1 ; x2 ; x3] [y1 ; y2 ; y3] = [(x1 , y1)  (x2 , y2) ; (x3, y3)]*)

exception UnequalLists;;

let rec zip (list1 : 'a list) (list2 : 'b list) : ('a * 'b) list = 
  match list1, list2 with 
  | [], [] -> []
  | (x :: xs), [] -> raise UnequalLists
  | [], (y :: ys) -> raise UnequalLists
  | x::xs, y::ys -> (x, y) :: zip(xs) (ys)

(* zip (xs) (ys)*)



(*
let xs = [1 ; 2 ; 3 ; 4];;

let ys = ["a" ; "b" ; "c" ; "d"];;


(* unzip [(x1 , y1)  (x2 , y2) ; (x3, y3)] = ([x1 ; x2 ; x3], [y1 ; y2 ; y3])*)
let unzip (list : ('a * 'b) list) : ('a list) * ('b list) = 
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
xs = [(2, "b"); (3, "c"); (4, "d")]*)

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


unzip (zip p) = p
let (a, b) = unzip xs in zip a b = xs
*)




(*sorting*)


(*
Optimal is 𝑂(𝑛 log 𝑛) comparisons
• Insertion sort: simple to code; too slow (quadratic) [174 secs]
• Quicksort: fast on average; quadratic in worst case [0.53 secs]
• Mergesort: optimal in theory; often slower than quicksort [1.4 secs]
• Match the algorithm to the application
*)

let rec insert (element : int) (list : int list) : int list = 
  match list with 
  | [] -> [element]
  | x::xs -> if element > x then
              (* x :: (insert element xs) *)
              x :: (insert element xs)
            else
            (* this is correct*)
              element::x::xs

let rec insert_sort (xs : int list) : int list = 
  match xs with
  | [] -> []
  | x::xs -> insert x (insert_sort xs)

(* insert_sort([3 ; 1 ; 4  ; 1  ; 5 ; 9])
quick_sort([3 ; 1 ; 4  ; 1  ; 5 ; 9])*)
let list = [3 ; 1 ; 4  ; 1  ; 5 ; 9]

let quick_sort (xs : int list) : int list = 
  match xs with
  | [] -> []
  | pivot::xs ->
    let rec partition (lessers_acc : int list) (greaters_acc : int list) (xs : int list) : (int list) * (int list) =
        match xs with
        | [] -> (lessers_acc, greaters_acc)
        | x::xs -> if x < pivot then
                    partition (x :: lessers_acc) greaters_acc xs
                else
                  partition lessers_acc (x :: greaters_acc) xs 
    in
        let (lessers, greaters) = partition [] [] xs in
        (quick_sort lessers) @ (pivot :: (quick_sort greaters))


let merge_sort (xs : int list) : int list = []
