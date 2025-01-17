(* non vuota, di 0 o 1*)

let rec lang1 l = match l with
  [] -> false
| '0'::[] -> true
| '1'::[] -> true
| x :: l' -> if x='0' || x='1' then lang1 l' else false
;;


let rec lang2_1 = match l with 
  [] -> true
| '1':: l' -> lang2_1 l'
| _ -> false
;;

let lang2 l = match l with 
  [] -> true
| '0' :: l' -> lang2_1 l'
| '1' :: l' -> lang2_1 l'
| _ -> false
;;


let rec lang3_1 l = match l with 
  '0'::[] -> true
| '0'::l' -> lang3_1 l'
| '1'::l' -> lang3_1 l'
| _ -> false
;;

let lang3 l = match l with 
  '0'::l' ->  lang3_1 l'
| _ -> false
;;


let rec lang4_2 l = match l with 
  '0'::l' -> lang4_2 l'
| [] -> true
| _ -> false
;;

let rec lang4_1 l = match l with 
  '0'::l' -> lang4_1 l'
| '1'::l' -> lang4_2 l'
| _ -> false
;;

let rec lang4 l = match l with 
  '0'::l' -> lang4 l'
| '1'::l' -> lang4_1 l'
| _ -> false
;;


let rec lang5_1 l = match l with
  [] -> true
| '1'::'1'::l' -> lang5_1 l'
| '0'::'0'::l' -> lang5_1 l'
;;

let lang5 l = match l with
  '1'::'1'::l' -> lang5_1 l'
| '0'::'0'::l' -> lang5_1 l'
| _ -> false
;;

let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
