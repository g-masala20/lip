{
  open Token
}

(* regex: *)
let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let consonant = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z' 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let atok = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*
let btok = ['a' 'e' 'i' 'o' 'u']+
let ctok = consonant* ['a' 'e' 'i' 'o' 'u']? consonant*
let dtok = ['-']? ['0'-'9']+ ['.']? ['0'-'9']*
let etok = ["0x" "0X"] ['0'-'9' 'A'-'F' 'a'-'f']+


rule read_token =
  parse
  | white { read_token lexbuf }  
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "="   { ASSIGN }
  | "+"   { PLUS }
  | ";"   { SEQ }  
  | atok  { ATOK (Lexing.lexeme lexbuf) }
  | btok  { BTOK (Lexing.lexeme lexbuf) }
  | ctok  { CTOK (Lexing.lexeme lexbuf) }
  | dtok  { DTOK (Lexing.lexeme lexbuf) }
  | etok  { ETOK (Lexing.lexeme lexbuf) }
  | id    { ID (Lexing.lexeme lexbuf) }
  | num   { CONST (Lexing.lexeme lexbuf) }    
  | eof   { EOF }
