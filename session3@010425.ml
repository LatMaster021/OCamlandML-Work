(*---------------------------------------------------------------------------------------------------------------------*)

(*#use "Session3@010425.ml";;*)

(*Covers Lecture 4: More on Lists*)
(*Focus on the All Ways of Making Change Algorithm*)

(*---------------------------------------------------------------------------------------------------------------------*)

(*make_change coins amount returns a list of coins from coins whose sum is amount or an error if not possible *)

(*
versions:
  1. Greedy Algorithm
  2. list to return ALL possible solutions
  3.  Use greedy, but backtrack if there's a failure
*)
  
(*create a custom exception for "out of coins"*)    
exception OutOfCoins;;
  
  let rec make_change_greedy (coins : int list) (amount : int) : int list = 
    match coins with
    | [] -> if amount = 0 then [] else raise OutOfCoins
    | x::xs -> if x <= amount then
      x :: make_change_greedy (coins) (amount - x)
    else
      make_change_greedy (xs) (amount)
  
(*
APPEND:
  [1 ; 2; 3] @ [ 4  ; 5 ;6] = [1 ; 2 ; 3 ; 4 ;5  ;6]
  
  CONS:
  1 :: [2 ; 3] = [1 ;  2 ; 3]
  
  [4;5;6] :: [[1; 2; 3]] =  [[4;5;6]  ; [1; 2; 3]] 
*)
  
(*
trying to make amount = 10
  x = 1
  
  [[2 ; 3 ;  4 ] ; [3 ; 6]]
  
  [[1  ; 2 ; 3 ; 4] ; [1  ; 3 ; 6]]
*)
  
  let rec map f = function
      | [] -> []
      | x::xs -> f x :: map f xs

(*
[[1; 2; 3]] @ [[1; 5]; [2; 4]]
  = [[1; 2; 3] ; [1; 5]; [2; 4] ]
  List.map : (f : 'a -> 'b) -> (xs : 'a list) -> 'b list 
*)
  
  let rec make_change_all (coins : int list) (amount : int) : int list list = 
    match coins with
    | [] -> if amount = 0 then [[]] else []
    | x::xs -> 
    if x <= amount then
      (*I want to prepend x to EACH of the possible results.  You need a do-for-each function on lists*)
      (*case 1: we use x*)
      List.map (List.cons x) (make_change_all (coins) (amount - x)) 
      @ 
      (*case 2: we don't use x*)
      make_change_all (xs) (amount)
    else
      make_change_all (xs) (amount)
(*
amount = 1
  coins = [100 ; 10; 5; 1]
*)

(*let rec make_change_all_2 (coins : int list) (amount : int) : int list list = 
    let less : int list = List.filter (fun coin -> coin <= amount) coins in 
      (*case 1: we use x*)
      List.map (List.cons x) (make_change_all_2 less (amount - x))
      @ 
      (*case 2: we don't use x*)
      make_change_all_2 less amount
*)
  
  
  let rec make_change_back (coins : int list) (amount : int) : int list = 
    match coins with
    | [] -> if amount = 0 then [] else raise OutOfCoins
    | x::xs -> if x <= amount then
      (*at this point: assume that we will use x, and if that raises an exception, backtrack to this point without using x*)
        try
        x :: make_change_back (coins) (amount - x)
        with 
        | OutOfCoins -> 
          make_change_back (xs) (amount)
    else
      make_change_back (xs) (amount)
  
  (*at this point: assume that we will use x, and if that raises an exception, backtrack to this point without using x*)
  
(*---------------------------------------------------------------------------------------------------------------------*)
