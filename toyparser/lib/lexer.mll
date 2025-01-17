{
  open Parser
}

let white = [' ' '\t']+
let num = '-'? ['0'-'9']|['1'-'9']['0'-'9']*
let hexstart = "0x"|"0X"
let hex = hexstart ['0'-'9' 'a'-'f' 'A'-'F'] | ['1'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | num { CONST (Lexing.lexeme lexbuf) }
  | hex { HEXCONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
