(*---------------------------------------------------------------------------------------------------------------------*)

(*#use "Session1@060325.ml";;*)

(*Covers Lecture 1: Introduction to Programming*)

(*---------------------------------------------------------------------------------------------------------------------*)

let succ x = 
  x + 1;;

let rec map f = function
  | [] -> []
  | x::xs -> f x :: map f xs  

let square x = 
  x * x;;

let rec power x n = 
  if n = 0 then
      1
  else
      if n mod 2 = 0 then
          square(power x (n/2))
      else
          x * power x (n - 1);;

let rec tpower x n acc = 
  if n = 0 then 
      acc
  else
      if n mod 2 = 0 then
          tpower (square x) (n/2) (acc)
      else
          tpower (x) (n - 1) (x * acc) ;;
let p x n = tpower x n 1;;

(*---------------------------------------------------------------------------------------------------------------------*)

(*Covers Lecture 2:Recursion and Efficiency*)

(*---------------------------------------------------------------------------------------------------------------------*)

let rec tsum n acc = 
  if n = 0 then
      acc
  else
      tsum (n - 1) (acc + n);;

let rec sum n = 
  if n = 0 then
      0
  else
      n + sum (n - 1);;

(*---------------------------------------------------------------------------------------------------------------------*)
