%token <int> INT
%token <int> WORD_CONST
%token <string> STRING
%token <string> SYMBOL
%token MODULE ASSIGN VAR IVAR LTLSPEC INVARSPEC INIT ESAC CASE NEXT ARRAY OF DEFINE
%token COLON_EQUAL COLON SEMI COMMA
%token MOD XOR XNOR PLUS MINUS TIMES DIV LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET
%token NOT_EQUAL EQUAL GREATER_EQUAL GREATER_THAN LESS_EQUAL
%token LESS_THAN NOT AND OR EOF
%token TRUE FALSE INTEGER BOOLEAN DOTDOT SELF
%token ABS MAX MIN WORD1 BOOL TOINT SIGNED UNSIGNED EXTEND RESIZE UNION
%token QUESTION IN COUNT WORD PROCESS FROZENVAR CONSTANTS TRANS INVAR
%token INIT_STMT ISA CTLSPEC SPEC NAME IMPLIES
%token EQUIV COMPUTE PSLSPEC ENDCASE ABORT FORALL ALWAYS NEVER EVENTUALLY
%token NEXT_BANG UNTIL UNTIL_BANG BEFORE BEFORE_BANG

%left OR XOR
%left AND
%left NOT
%left EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%left PLUS MINUS
%left TIMES DIV MOD

%start smv_module
%type <Ast.program_type> smv_module
%%

smv_module: MODULE SYMBOL vars_section define_section ASSIGN assigns ltls EOF
    { Ast.Program ($2, $3, $4, $6, $7) };

vars_section: VAR vars { $2 }
  | { [] }
;

define_section: DEFINE definitions { $2 }
  | { [] }
;

definition: lvalue COLON_EQUAL expr SEMI { Ast.Definition ($1, $3) };

definitions: definition definitions { $1 :: $2 }
  | { [] }
;

vars: var_decl vars { $1 :: $2 }
  | { [] }
;

enum_list: SYMBOL COMMA enum_list { $1 :: $3 }
    | SYMBOL { [$1] }
  | { [] }
;

deftype: INTEGER { Array_Int } | BOOLEAN { Array_Bool } |
  INT DOTDOT INT { Array_Range ($1,$3) }

var_decl: SYMBOL COLON BOOLEAN SEMI { Ast.Bool_Decl $1 }
  | SYMBOL COLON INTEGER SEMI { Ast.Int_Decl $1 }
  | SYMBOL COLON INT DOTDOT INT SEMI { Ast.Int_Range_Decl ($1, $3, $5) }
  | SYMBOL COLON LBRACE enum_list RBRACE SEMI { Ast.Enum_Decl ($1, $4) }
  | SYMBOL COLON ARRAY INT DOTDOT INT OF deftype SEMI
    {Ast.Array_Decl ($1, $4, $6, $8) }
;

expr: TRUE { Ast.Expr_Bool true }
  | FALSE { Ast.Expr_Bool false }
  | INT { Ast.Expr_Int $1 }
  | LBRACKET expr_list RBRACKET { Ast.Expr_Array $2 }
  | expr LBRACKET expr RBRACKET { Ast.Expr_Array_Ref ($1,$3) }
  | SYMBOL LPAREN expr_list RPAREN { Ast.Expr_Func ($1, $3) }
  | SYMBOL { Ast.Expr_Var $1 }
  | LPAREN expr RPAREN { $2 }
  | MINUS expr { Ast.Expr_Neg $2 }
  | expr PLUS expr { Ast.Expr_Add ($1 ,$3) }
  | expr MINUS expr { Ast.Expr_Sub ($1, $3) }
  | expr TIMES expr { Ast.Expr_Mul ($1, $3) }
  | expr DIV expr { Ast.Expr_Div ($1, $3) }
  | expr MOD expr { Ast.Expr_Mod ($1, $3) }
  | NOT expr { Ast.Expr_Not $2 }
  | expr AND expr { Ast.Expr_And ($1, $3) }
  | expr OR expr { Ast.Expr_Or ($1, $3) }
  | expr XOR expr { Ast.Expr_Xor ($1, $3) }
  | expr EQUAL expr { Ast.Expr_Eq ($1, $3) }
  | expr NOT_EQUAL expr { Ast.Expr_Ne ($1, $3) }
  | expr LESS_THAN expr { Ast.Expr_Lt ($1, $3) }
  | expr LESS_EQUAL expr { Ast.Expr_Le ($1, $3) }
  | expr GREATER_THAN expr { Ast.Expr_Gt ($1, $3) }
  | expr GREATER_EQUAL expr { Ast.Expr_Ge ($1, $3) }
;

expr_list: expr COMMA expr_list { $1 :: $3 }
      | expr { [ $1 ] }
  | { [] }
;

case: expr COLON LBRACE expr_list RBRACE SEMI { Ast.Case ($1, Ast.Choose $4) } 
      | expr COLON expr SEMI { Ast.Case ($1, Ast.Single $3) }
;

cases: case cases { $1 :: $2 }
  | { [] }
;

stmt_val: CASE cases ESAC { Ast.Val_Case $2 }
  | expr { Ast.Val_Expr $1 }
;

lvalue: SYMBOL LBRACKET expr RBRACKET { Ast.LVal_ARef ($1,$3) }
  | SYMBOL { Ast.LVal_Var $1 }
;

next : NEXT LPAREN lvalue RPAREN COLON_EQUAL stmt_val SEMI { Ast.Next ($3, $6) };
init: INIT LPAREN lvalue RPAREN COLON_EQUAL stmt_val SEMI { Ast.Init ($3, $6) }

assign: next { $1 }
  | init { $1 }
;

assigns: assign assigns { $1 :: $2 }
  | { [] }
;

 opt_semi: SEMI {}
  | {}
;

ltl: LTLSPEC SYMBOL expr opt_semi { Ast.Ltl ($2,$3) }
  | INVARSPEC expr opt_semi { Ast.Invarspec $2 }
;
					 
  ltls: ltl ltls { $1 :: $2 }
    | { [] }
;
