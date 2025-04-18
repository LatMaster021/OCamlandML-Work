(*---------------------------------------------------------------------------------------------------------------------*)

(*#use "Session2@130325.ml";;*)

(*Covers Lecture 3: Lists*)

(*---------------------------------------------------------------------------------------------------------------------*)

let calculate a = 
  let rec length acc = function
  | [] -> acc
  | hd::tl -> length (acc + 1) tl in
  length 0 a;;


let rec append a b = 
  match a with
  | [] -> b
  | hd::tl -> hd :: (append (tl) (b))

let rec tappend a b acc = 
  match a, b with
  | [], [] -> List.rev(acc)
  | a, [] -> a
  | [], y::ys -> tappend ([]) (ys) (y::acc)
  | x::xs, b -> tappend (xs) (b) (x::acc)


  let rec append a b = 
    match a, b with 
    | [], [] -> []
    | lst, [] -> lst
    | [], lst -> lst
    | y::ys, lst -> append ys (y::lst) 

(*---------------------------------------------------------------------------------------------------------------------*)
