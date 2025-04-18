(*---------------------------------------------------------------------------------------------------------------------*)

(*#use "Session5@150425.ml";;*)

(*Covers Lecture 5: Sorting*)

(*---------------------------------------------------------------------------------------------------------------------*)

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

(* 
insert_sort([3 ; 1 ; 4  ; 1  ; 5 ; 9])
quick_sort([3 ; 1 ; 4  ; 1  ; 5 ; 9])
*)

let list = [3 ; 1 ; 4  ; 1  ; 5 ; 9]

let rec quick_sort (xs : int list) : int list = 
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

(*
Merge two lists.
If l1 and l2 are sorted, then 
merge l1 l2 should also be sorted.
*)

let rec merge (l1 : int list) (l2 : int list) : int list = 
  match l1, l2  with
  | [], _ -> l2
  | _, [] -> l1
  | x :: xs, y :: ys -> 
    if x > y then
      y::merge (x::xs) ys
    else
      x::merge xs (y::ys)

let l1 : int list = [1 ; 2 ;3];;
let l2 : int list = [4 ; 5 ; 6];;
let l3 : int list = merge l1 l2;;

(*
merge [1 ; 1 ; 1] [10 ;  10 ;  10] => [1, 10, 1, 10, 1, 10]
*)

(*
Length of a List
https://ocaml.org/manual/5.3/api/List.html
List.length [1  ;2 ; 3]
*)

(*
Merge Sort
0. If the list has length 0 or 1, just return that list
1. split the list into two (approximately) equal lengths
2. recursively sort each sub-list
3. merge back together the sorted results
*)

(*
Hint: use List.take and List.drop and List.length
List.drop : int -> 'a list -> 'a list
List.take : int -> 'a list -> 'a list
List.length : 'a list -> int
https://ocaml.org/manual/5.3/api/List.html
*)

(*
List.take 3 [11 ; 12 ; 13 ; 14 ;15] => [11 ; 12 ; 13]
List.drop 3 [11 ; 12 ; 13 ; 14 ;15] => [14 ; 15]
*)

let rec take i = function 
  | []-> [] 
  | x::xs-> if i > 0 then x :: take (i - 1) xs else []

let rec drop i = function 
  | []-> [] 
  | x::xs-> if i > 0 then 
      drop (i-1) xs 
    else 
      x::xs

let rec merge_sort (xs : int list) : int list =
  match xs with
  | [x] -> xs
  | [] -> []
  | _ :: _ -> 
  let half = (List.length xs) / 2 in
  merge (merge_sort (drop half xs)) (merge_sort (take half xs))

(*
let l4 : int list = [ 5 ; 7 ;2 ; 3 ; 1  ; 4]

let l5 : int list = merge_sort l4

merge_sort [ 5 ; 7 ;2 ; 3 ; 1  ; 4];;
*)

(*---------------------------------------------------------------------------------------------------------------------*)
