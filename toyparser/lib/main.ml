open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

  type int_or_err = (int, string) Result.t

let ( ==> ) (res : int_or_err) (f : int -> int_or_err) : int_or_err =
  match res with
  | Ok value -> f value
  | Error msg -> Error msg

let string_of_intorerr : int_or_err -> string = function
  | Ok n -> string_of_int n
  | Error msg -> msg

(*-----------*)

let interr_of_int x = Ok(x);;

let add_eval (e1, e2) = match ( (e1), (e2)) with  
  | (Ok(v1), Ok(v2)) -> Ok(v1 + v2)
  | _ -> Error "erroreADDgeneric"
;;

let sub_eval (e1, e2) = match ( (e1), (e2)) with  
  | (Ok(v1), Ok(v2)) -> Ok(v1 - v2)
  | _ -> Error "erroreSUBgeneric"
;;

let mul_eval (e1, e2) = match ( (e1), (e2)) with  
  | (Ok(v1), Ok(v2)) -> Ok(v1 * v2)
  | _ -> Error "erroreMULgeneric"
;;

let div_eval (e1, e2) = match ( (e1), (e2)) with  
  | (_, Ok(v2)) when v2 = 0 -> Error "DivisionByZero"
  | (Ok(v1), Ok(v2)) -> Ok(v1 / v2)
  | _ -> Error "erroreDIVgeneric"
;;

let rec eval : ast -> int_or_err = function
  | Const n -> Ok n
  | Add (e1, e2) -> add_eval ((eval e1 ),(eval e2))
  | Sub (e1, e2) -> sub_eval ((eval e1 ),(eval e2))
  | Mul (e1, e2) -> mul_eval ((eval e1 ),(eval e2))
  | Div (e1, e2) -> div_eval ((eval e1 ),(eval e2))
;;

