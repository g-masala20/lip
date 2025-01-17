type token =
  | LPAREN          (* ( *)
  | RPAREN          (* ) *)
  | ASSIGN          (* = *)
  | PLUS            (* + *)
  | SEQ             (* ; *)
  | ID of string    (* identifier *)
  | CONST of string (* constant *)
  | ATOK of string  (* all strings of letters and digits starting by a capital letter *)
  | BTOK of string  (*all strings of lowercase vowels*)
  | CTOK of string  (*all strings of letters containing at most one vowel*)
  | DTOK of string  (*all strings of digits possibly starting with - and possibly containing a . followed by other digits (e.g., 3.14, -7., -.3)*)
  | ETOK of string  (*all strings representing hexadecimal numbers in C syntax (i.e., strings starting with 0x or 0X, and containing hexadecimal value HH, where HH is 1 or more hex digits, '0'-'9','A'-'F','a'-'f')*)
  | EOF             (* end of file *)

let string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | SEQ -> "SEQ"
  | ID(s) -> "ID(" ^ s ^ ")"
  | CONST(s) -> "CONST(" ^ s ^ ")"
  | ATOK(s) -> "ATOK(" ^ s ^ ")"
  | BTOK(s) -> "BTOK(" ^ s ^ ")"
  | CTOK(s) -> "CTOK(" ^ s ^ ")"
  | DTOK(s) -> "DTOK(" ^ s ^ ")"
  | ETOK(s) -> "ETOK(" ^ s ^ ")"
  | EOF -> "EOF"
