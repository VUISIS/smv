{
open Smv_parser

let word_const_regex = Str.regexp "0[us]?[bBoOdDhH][0-9]+_[0-9a-fA-F_]+"

let parse_word_constant str =
  if Str.string_match word_const_regex str 0 then
    0
  else
    failwith ("Invalid word constant " ^ str)

}

rule token = parse
  | '-''-'[^'\n']*['\n'] { Lexing.new_line lexbuf; token lexbuf }
  | ['\n'] { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t'] { token lexbuf }
  | "0" ['u' 's']? ['b' 'B' 'o' 'O' 'd' 'D' 'h' 'H'] ['0'-'9']+ "_"
    ['0'-'9' 'a'-'f' 'A'-'F' '_']+ as lxm { WORD_CONST (parse_word_constant lxm) }
  | '-'? ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | "MODULE" { MODULE }
  | "ASSIGN" { ASSIGN }
  | "VAR" { VAR }
  | "IVAR" { IVAR }
  | "FROZENVAR" { FROZENVAR }
  | "DEFINE" { DEFINE }
  | "CONSTANTS" { CONSTANTS }
  | "TRANS" { TRANS }
  | "INIT" { INIT }
  | "INVAR" { INVAR }
  | "LTLSPEC" { LTLSPEC }
  | "INVARSPEC" { INVARSPEC }
  | "ISA" { ISA }
  | "CTLSPEC" { CTLSPEC }
  | "COMPUTE" { COMPUTE }
  | "FAIRNESS" { FAIRNESS }
  | "JUSTICE" { JUSTICE }
  | "COMPASSION" { COMPASSION }
  | "SPEC" { SPEC }
  | "NAME" { NAME }
  | "TRUE" { TRUE }
  | "FALSE" { FALSE }
  | "EBF" { EBF }
  | "ABF" { ABF }
  | "EBG" { EBG }
  | "ABG" { ABG }
  | "BU" { BU }
  | "EG" { EG }
  | "EX" { EX }
  | "EF" { EF }
  | "AG" { AG }
  | "AX" { AX }
  | "AF" { AF }
  | "E" { E }
  | "U" { U }
  | "A" { A }
  | "X" { X }
  | "G" { G }
  | "F" { F }
  | "V" { V }
  | "Y" { Y }
  | "Z" { Z }
  | "H" { H }
  | "O" { O }
  | "S" { S }
  | "T" { T }
  | "boolean" { BOOLEAN }
  | "word" { WORD }
  | "array" { ARRAY }
  | "of" { OF }
  | "abs" { ABS }
  | "max" { MAX }
  | "min" { MIN }
  | "init" { INIT }
  | "next" { NEXT }
  | "case" { CASE }
  | "esac" { ESAC }
  | "mod" { MOD }
  | "self" { SELF }
  | "xor" { XOR }
  | "xnor" { XNOR }
  | "word1" { WORD1 }
  | "bool" { BOOL }
  | "toint" { TOINT }
  | "signed" { SIGNED }
  | "unsigned" { UNSIGNED }
  | "extend" { EXTEND }
  | "resize" { RESIZE }
  | "union" { UNION }
  | "in" { IN }
  | "count" { COUNT }
  | "process" { PROCESS }
  | "sizeof" { SIZEOF }
  
  | ['A'-'Z' 'a'-'z' '_' ]['A'-'Z' 'a'-'z' '0'-'9' '_' '$' '#' '-']* as lxm { SYMBOL(lxm) }
  | ":=" { COLON_EQUAL }
  | "::" { COLON_COLON }
  | ':' { COLON }
  | ';' { SEMI }
  | ',' { COMMA }
  | '+' { PLUS }
  | "->" { IMPLIES }
  | "<->" { EQUIV }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "!=" { NOT_EQUAL }
  | '=' { EQUAL }
  | ">>" { RIGHT_SHIFT }
  | "<<" { LEFT_SHIFT }
  | ">=" { GREATER_EQUAL }
  | '>' { GREATER_THAN }
  | "<=" { LESS_EQUAL }
  | '<' { LESS_THAN }
  | '!' { NOT }
  | '&' { AND }
  | '|' { OR }
  | ".." { DOTDOT }
  | '?' { QUESTION }
  | '.' { DOT }
  | eof { EOF }  
