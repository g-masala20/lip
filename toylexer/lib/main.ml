open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* -----------------------------------------------------------------------------------------------------*)

  let rec count e l = match l with
  | [] -> 0
  | h::t -> if h = e then 1 + count e t else count e t
;;

let rec gen_lista_freq list_token = match list_token with 
    [] -> []
  | a::l' -> (a, count a list_token)::(gen_lista_freq l')
;;



let rec elimina_dopp a lista_copp = match lista_copp with
    [] -> []
  | e::l' -> if a = (fst e) then (elimina_dopp a l') else e::(elimina_dopp a l')
;;

let rec pulisci_lista lista_copp = match lista_copp with 
    [] -> []
  | (a,b)::l' -> (a,b):: pulisci_lista(elimina_dopp a l')
;;


(* frequency : int -> 'a list -> ('a * int) list *)
let rec frequency list_token = gen_lista_freq list_token |> pulisci_lista;;

(* -----------------------------------------------------------------------------------------------------*)