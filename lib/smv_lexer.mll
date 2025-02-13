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
  | "0" ['u' 's']? ['b' 'B' 'o' 'O' 'd' 'D' 'h' 'H'] ['0'-'9']+ "_" ['0'-'9' 'a'-'f' 'A'-'F' '_']+ as lxm { WORD_CONST (parse_word_constant lxm) }
  | '-'? ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '"'[^'"']*'"' as lxm { STRING(String.sub lxm 1 ((String.length lxm) - 2)) }
  | "MODULE" { MODULE }
  | "ASSIGN" { ASSIGN }
  | "VAR" { VAR }
  | "IVAR" { IVAR }
  | "FROZENVAR" { FROZENVAR }
  | "DEFINE" { DEFINE }
  | "CONSTANTS" { CONSTANTS }
  | "TRANS" { TRANS }
  | "INIT" { INIT_STMT }
  | "INVAR" { INVAR }
  | "LTLSPEC" { LTLSPEC }
  | "INVARSPEC" { INVARSPEC }
  | "ISA" { ISA }
  | "CTLSPEC" { CTLSPEC }
  | "COMPUTE" { COMPUTE }
  | "PSLSPEC" { PSLSPEC }
  | "SPEC" { SPEC }
  | "NAME" { NAME }
  | "TRUE" { TRUE }
  | "FALSE" { FALSE }
  | "integer" { INTEGER }
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
  | "endcase" { ENDCASE }
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
  | "abort" { ABORT }
  | "forall" { FORALL }
  | "always" { ALWAYS }
  | "never" { NEVER }
  | "eventually!" { EVENTUALLY }
  | "next!" { NEXT_BANG }
  | "until" { UNTIL }
  | "until!" { UNTIL_BANG }
  | "before" { BEFORE }
  | "before!" { BEFORE_BANG }
  
  | ['A'-'Z' 'a'-'z' '_' ]['A'-'Z' 'a'-'z' '0'-'9' '_' '$' '#' '-']* as lxm { SYMBOL(lxm) }
  | ":=" { COLON_EQUAL }
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
  | ">=" { GREATER_EQUAL }
  | '>' { GREATER_THAN }
  | "<=" { LESS_EQUAL }
  | '<' { LESS_THAN }
  | '!' { NOT }
  | '&' { AND }
  | '|' { OR }
  | ".." { DOTDOT }
  | '?' { QUESTION }
  | eof { EOF }
  
