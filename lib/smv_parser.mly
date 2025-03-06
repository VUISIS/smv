%token <int> INT
%token <int> WORD_CONST
%token <string> SYMBOL
%token MODULE ASSIGN VAR IVAR LTLSPEC INVARSPEC INIT ESAC CASE NEXT ARRAY OF DEFINE
%token COLON_EQUAL COLON SEMI COMMA
%token MOD XOR XNOR PLUS MINUS TIMES DIV LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET
%token NOT_EQUAL EQUAL GREATER_EQUAL GREATER_THAN LESS_EQUAL
%token LESS_THAN NOT AND OR
%token TRUE FALSE BOOLEAN DOTDOT SELF
%token ABS MAX MIN WORD1 BOOL TOINT SIGNED UNSIGNED EXTEND RESIZE UNION
%token QUESTION IN COUNT WORD PROCESS FROZENVAR CONSTANTS TRANS INVAR
%token INIT_STMT ISA CTLSPEC SPEC NAME IMPLIES
%token EQUIV COMPUTE
%token DOT COLON_COLON
%token RIGHT_SHIFT LEFT_SHIFT
%token EG EX EF AG AX AF E U A X G F V Y Z H O S T
%token EBF ABF EBG ABG BU
%token FAIRNESS JUSTICE COMPASSION

%right IMPLIES
%left EQUIV
%left QUESTION
%left OR XOR XNOR
%left AND
%left EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%left IN
%left UNION
%left LEFT_SHIFT RIGHT_SHIFT
%left PLUS MINUS
%left TIMES DIV MOD
%left COLON_COLON
%left NOT

%start<Smv.module_type> smv_module
%%

complex_identifier: SYMBOL { Smv.IdSym $1 }
  | complex_identifier DOT SYMBOL { Smv.IdDot ($1, $3) }
  | complex_identifier LBRACKET simple_expr RBRACKET { Smv.IdRef ($1, $3) }
  | SELF { Smv.IdSelf}
;

variable_identifier: complex_identifier { $1};

define_identifier: complex_identifier { $1};

boolean_constant: TRUE { Smv.ConstBool true }
  | FALSE { Smv.ConstBool false }
;

integer_constant: INT { Smv.ConstInt $1};

symbolic_constant: complex_identifier { Smv.ConstSym $1};

word_constant: WORD_CONST { Smv.ConstWord $1 };

range_constant: INT DOTDOT INT { Smv.ConstRange ($1, $3) };

constant: boolean_constant {$1}
  | integer_constant {$1}
  | symbolic_constant {$1}
  | word_constant {$1}
  | range_constant {$1}
;

basic_expr: constant {ExprConst $1}
  | variable_identifier {ExprVar $1}
  | define_identifier {ExprDef $1}
  | LPAREN basic_expr RPAREN {$2}
  | ABS basic_expr {ExprAbs $2}
  | MAX LPAREN basic_expr COMMA basic_expr RPAREN {ExprMax ($3, $5) }
  | MIN LPAREN basic_expr COMMA basic_expr RPAREN {ExprMin ($3, $5) }
  | NOT basic_expr {ExprNot $2}
  | basic_expr AND basic_expr {ExprAnd ($1, $3) }
  | basic_expr OR basic_expr {ExprOr ($1, $3)}
  | basic_expr XOR basic_expr {ExprXor ($1, $3)}
  | basic_expr XNOR basic_expr {ExprXnor ($1, $3)}
  | basic_expr IMPLIES basic_expr {ExprImplies ($1, $3)}
  | basic_expr EQUIV basic_expr {ExprEquiv ($1, $3)}
  | basic_expr EQUAL basic_expr {}
  | basic_expr NOT_EQUAL basic_expr {}
  | basic_expr LESS_THAN basic_expr {}
  | basic_expr GREATER_THAN basic_expr {}
  | basic_expr LESS_EQUAL basic_expr {}
  | basic_expr GREATER_EQUAL basic_expr {}
  | MINUS basic_expr {}
  | basic_expr PLUS basic_expr {}
  | basic_expr MINUS basic_expr {}
  | basic_expr TIMES basic_expr {}
  | basic_expr DIV basic_expr {}
  | basic_expr MOD basic_expr {}
  | basic_expr RIGHT_SHIFT basic_expr {}
  | basic_expr LEFT_SHIFT basic_expr {}
  | basic_expr LBRACKET basic_expr RBRACKET {}
  | basic_expr LBRACKET INT COLON INT RBRACKET {}
  | basic_expr COLON_COLON basic_expr {}
  | WORD1 LPAREN basic_expr RPAREN {}
  | BOOL LPAREN basic_expr RPAREN {}
  | TOINT LPAREN basic_expr RPAREN {}
  | SIGNED LPAREN basic_expr RPAREN {}
  | UNSIGNED LPAREN basic_expr RPAREN {}
  | EXTEND LPAREN basic_expr COMMA basic_expr RPAREN {}
  | RESIZE LPAREN basic_expr COMMA basic_expr RPAREN {}
  | basic_expr UNION basic_expr {}
  | LBRACE set_body_expr RBRACE {}
  | basic_expr IN basic_expr {}
  | basic_expr QUESTION basic_expr COLON basic_expr {}
  | COUNT LPAREN basic_expr_list RPAREN {}
  | case_expr {}
  | NEXT LPAREN basic_expr RPAREN {}
;

basic_expr_list: basic_expr COMMA basic_expr_list {}
  | basic_expr {}
  | {}
;

set_body_expr: basic_expr COMMA set_body_expr {}
  | basic_expr {}
  | {}
;

simple_type_specifier: BOOLEAN {}
  | WORD LBRACKET INT RBRACKET {}
  | UNSIGNED WORD LBRACKET INT RBRACKET {}
  | SIGNED WORD LBRACKET INT RBRACKET {}
  | LBRACE enumeration_type_body RBRACE {}
  | INT DOTDOT INT {}
  | ARRAY INT DOTDOT INT OF simple_type_specifier {}
;

enumeration_type_body: enumeration_type_value COMMA enumeration_type_body {}
  | enumeration_type_value {}
  | {}
;

enumeration_type_value: SYMBOL {}
  | INT {}
;

module_type_specifier: SYMBOL opt_parameter_list {}
  | PROCESS SYMBOL opt_parameter_list {}
;

type_specifier: simple_type_specifier {}
  | module_type_specifier {}
;

parameter_list: simple_expr COMMA parameter_list {}
  | simple_expr {}
  | {}
;

opt_parameter_list: LPAREN parameter_list RPAREN {}
  | {}
;

simple_expr: basic_expr {}
;

next_expr: basic_expr {}
;

case_body: basic_expr COLON basic_expr SEMI case_body {}
;

case_expr: CASE case_body ESAC {}
;

var_list: complex_identifier COLON type_specifier var_list {}
  | {}
;


simple_var_list: complex_identifier COLON simple_type_specifier SEMI simple_var_list {}
  | {}
;

var_declaration: VAR var_list {}
;

ivar_declaration: IVAR simple_var_list {}
;

frozenvar_declaration: FROZENVAR simple_var_list {}
;

define_body: complex_identifier COLON_EQUAL next_expr SEMI define_body {}
  | {}
;

define_declaration: DEFINE define_body {}
;

constants_body: complex_identifier COMMA constants_body {}
  | complex_identifier {}
  | {}
;

constants_declaration: CONSTANTS constants_body {}
;

assign: complex_identifier COLON_EQUAL simple_expr {}
  | INIT LPAREN complex_identifier RPAREN COLON_EQUAL simple_expr {}
  | NEXT LPAREN complex_identifier RPAREN COLON_EQUAL simple_expr {}
;

assign_list: assign assign_list {}
  | {}
;

assign_constraint: ASSIGN assign_list {}

opt_semi: SEMI {}
  | {}
;

trans_constraint: TRANS next_expr opt_semi {}
;

init_constraint: INIT_STMT simple_expr opt_semi {}
;

invar_constraint: INVAR simple_expr opt_semi {}
;

isa_declaration: ISA SYMBOL {}
;

ctl_specification: CTLSPEC ctl_expr SEMI {}
  | SPEC ctl_expr opt_semi {}
  | CTLSPEC NAME SYMBOL COLON_EQUAL ctl_expr opt_semi {}
  | SPEC NAME SYMBOL COLON_EQUAL ctl_expr opt_semi {}
;

ctl_expr: simple_expr {}
  | LPAREN ctl_expr RPAREN {}
  | NOT ctl_expr {}
  | ctl_expr AND ctl_expr {}
  | ctl_expr OR ctl_expr {}
  | ctl_expr XOR ctl_expr {}
  | ctl_expr XNOR ctl_expr {}
  | ctl_expr IMPLIES ctl_expr {}
  | ctl_expr EQUIV ctl_expr {}
  | EG ctl_expr {}
  | EX ctl_expr {}
  | EF ctl_expr {}
  | AG ctl_expr {}
  | AX ctl_expr {}
  | AF ctl_expr {}
  | E LBRACKET ctl_expr U ctl_expr RBRACKET {}
  | A LBRACKET ctl_expr U ctl_expr RBRACKET {}
;

invar_specification: INVARSPEC next_expr SEMI {}
  | INVARSPEC NAME SYMBOL COLON_EQUAL next_expr opt_semi {}
;

ltl_specification: LTLSPEC ltl_expr opt_semi {}
  | LTLSPEC NAME SYMBOL COLON_EQUAL ltl_expr opt_semi {}
;

ltl_expr: next_expr {}
  | LPAREN ltl_expr RPAREN {}
  | NOT ltl_expr {}
  | ltl_expr AND ltl_expr {}
  | ltl_expr OR ltl_expr {}
  | ltl_expr XOR ltl_expr {}
  | ltl_expr IMPLIES ltl_expr {}
  | ltl_expr EQUIV ltl_expr {}
  | X ltl_expr {}
  | G ltl_expr {}
  | F ltl_expr {}
  | ltl_expr U ltl_expr {}
  | ltl_expr V ltl_expr {}
  | Y ltl_expr {}
  | Z ltl_expr {}
  | H ltl_expr {}
  | O ltl_expr {}
  | ltl_expr S ltl_expr {}
  | ltl_expr T ltl_expr {}
;

rtctl_specification: CTLSPEC rtctl_expr opt_semi {}
  | SPEC rtctl_expr opt_semi {}
  | CTLSPEC NAME SYMBOL COLON_EQUAL rtctl_expr opt_semi {}
  | SPEC NAME SYMBOL COLON_EQUAL rtctl_expr opt_semi {}
;

range: INT DOTDOT INT {}
;

rtctl_expr: ctl_expr {}
  | EBF range rtctl_expr {}
  | ABF range rtctl_expr {}
  | EBG range rtctl_expr {}
  | ABG range rtctl_expr {}
  | A LBRACKET rtctl_expr BU range rtctl_expr RBRACKET {}
  | E LBRACKET rtctl_expr BU range rtctl_expr RBRACKET {}
;

compute_specification: COMPUTE compute_expr opt_semi {}
  | COMPUTE NAME SYMBOL COLON_EQUAL compute_expr opt_semi {}
;

compute_expr: MIN LBRACKET rtctl_expr COMMA rtctl_expr RBRACKET {}
  | MAX LBRACKET rtctl_expr COMMA rtctl_expr RBRACKET {}
;

fairness_constraint: FAIRNESS simple_expr opt_semi {}
  | JUSTICE simple_expr opt_semi {}
  | COMPASSION LPAREN simple_expr COMMA simple_expr RPAREN opt_semi {}
;

module_element: var_declaration {}
  | ivar_declaration {}
  | frozenvar_declaration {}
  | define_declaration {}
  | constants_declaration {}
  | assign_constraint {}
  | trans_constraint {}
  | init_constraint {}
  | invar_constraint {}
  | fairness_constraint {}
  | ctl_specification {}
  | rtctl_specification {}
  | invar_specification {}
  | ltl_specification {}
  | compute_specification {}
  | isa_declaration {}
;

module_parameters: SYMBOL {}
  | module_parameters COMMA SYMBOL {}
;

opt_module_parameters: LPAREN module_parameters RPAREN {}
  | {}
;

module_body: module_element {}
  | module_body module_element {}
;

opt_module_body: module_body {}
  | {}
;

smv_module: MODULE SYMBOL opt_module_parameters opt_module_body {}
;
