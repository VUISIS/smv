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
%token ABS MAX MIN WORD1 BOOL TOINT SIGNED UNSIGNED EXTEND RESIZE UNION SIZEOF
%token QUESTION IN COUNT WORD PROCESS FROZENVAR CONSTANTS TRANS INVAR
%token INIT_STMT ISA CTLSPEC SPEC NAME IMPLIES
%token EQUIV COMPUTE
%token DOT COLON_COLON
%token RIGHT_SHIFT LEFT_SHIFT
%token EG EX EF AG AX AF E U A X G F V Y Z H O S T
%token EBF ABF EBG ABG BU
%token FAIRNESS JUSTICE COMPASSION
%token EOF

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

boolean_constant: TRUE { Smv.ConstBool true }
  | FALSE { Smv.ConstBool false }
;

integer_constant: INT { Smv.ConstInt $1};

word_constant: WORD_CONST { Smv.ConstWord $1 };

range_constant: INT DOTDOT INT { Smv.ConstRange ($1, $3) };

constant: boolean_constant {$1}
  | integer_constant {$1}
  | word_constant {$1}
  | range_constant {$1}
;

basic_expr: constant {Smv.ExprConst $1}
  | variable_identifier {Smv.ExprVar $1}
  | LPAREN basic_expr RPAREN {$2}
  | ABS basic_expr {Smv.ExprAbs $2}
  | MAX LPAREN basic_expr COMMA basic_expr RPAREN {Smv.ExprMax ($3, $5) }
  | MIN LPAREN basic_expr COMMA basic_expr RPAREN {Smv.ExprMin ($3, $5) }
  | NOT basic_expr {Smv.ExprNot $2}
  | basic_expr AND basic_expr {Smv.ExprAnd ($1, $3) }
  | basic_expr OR basic_expr {Smv.ExprOr ($1, $3)}
  | basic_expr XOR basic_expr {Smv.ExprXor ($1, $3)}
  | basic_expr XNOR basic_expr {Smv.ExprXnor ($1, $3)}
  | basic_expr IMPLIES basic_expr {Smv.ExprImplies ($1, $3)}
  | basic_expr EQUIV basic_expr {Smv.ExprEquiv ($1, $3)}
  | basic_expr EQUAL basic_expr {Smv.ExprEqual ($1, $3)}
  | basic_expr NOT_EQUAL basic_expr {Smv.ExprNotEqual ($1, $3)}
  | basic_expr LESS_THAN basic_expr {Smv.ExprLess ($1, $3)}
  | basic_expr GREATER_THAN basic_expr {Smv.ExprGreater ($1, $3)}
  | basic_expr LESS_EQUAL basic_expr {Smv.ExprLessEqual ($1, $3)}
  | basic_expr GREATER_EQUAL basic_expr {Smv.ExprGreaterEqual ($1, $3)}
  | MINUS basic_expr {Smv.ExprNeg $2}
  | basic_expr PLUS basic_expr {Smv.ExprPlus ($1, $3)}
  | basic_expr MINUS basic_expr {Smv.ExprMinus ($1, $3)}
  | basic_expr TIMES basic_expr {Smv.ExprTimes ($1, $3)}
  | basic_expr DIV basic_expr {Smv.ExprDiv ($1, $3)}
  | basic_expr MOD basic_expr {Smv.ExprMod ($1, $3)}
  | basic_expr RIGHT_SHIFT basic_expr {Smv.ExprShiftRight ($1, $3)}
  | basic_expr LEFT_SHIFT basic_expr {Smv.ExprShiftLeft ($1, $3)}
  | basic_expr LBRACKET basic_expr RBRACKET {Smv.ExprArrayRef ($1, $3)}
  | basic_expr LBRACKET INT COLON INT RBRACKET {Smv.ExprBitSel ($1, $3, $5)}
  | basic_expr COLON_COLON basic_expr {Smv.ExprConcat ($1, $3)}
  | WORD1 LPAREN basic_expr RPAREN {Smv.ExprWord1 $3}
  | BOOL LPAREN basic_expr RPAREN {Smv.ExprBool $3}
  | TOINT LPAREN basic_expr RPAREN {Smv.ExprToInt $3}
  | SIGNED LPAREN basic_expr RPAREN {Smv.ExprSigned $3}
  | SIZEOF LPAREN basic_expr RPAREN {Smv.ExprSizeof $3}
  | UNSIGNED LPAREN basic_expr RPAREN {Smv.ExprUnsigned $3}
  | EXTEND LPAREN basic_expr COMMA basic_expr RPAREN {Smv.ExprExtend ($3, $5)}
  | RESIZE LPAREN basic_expr COMMA basic_expr RPAREN {Smv.ExprResize ($3, $5)}
  | basic_expr UNION basic_expr {Smv.ExprUnion ($1, $3)}
  | LBRACE set_body_expr RBRACE {Smv.ExprSet $2}
  | basic_expr IN basic_expr {Smv.ExprIn ($1, $3)}
  | basic_expr QUESTION basic_expr COLON basic_expr {Smv.ExprTernary ($1, $3, $5)}
  | COUNT LPAREN basic_expr_list RPAREN {Smv.ExprCount $3}
  | case_expr {$1}
  | NEXT LPAREN basic_expr RPAREN {Smv.ExprNext $3}
;

basic_expr_list: basic_expr COMMA basic_expr_list {$1 :: $3}
  | basic_expr {[$1]}
  | {[]}
;

set_body_expr: basic_expr COMMA set_body_expr {$1 :: $3}
  | basic_expr {[$1]}
  | {[]}
;

simple_type_specifier: BOOLEAN {Smv.TypeBoolean}
  | WORD LBRACKET INT RBRACKET {Smv.TypeWord $3}
  | UNSIGNED WORD LBRACKET INT RBRACKET {Smv.TypeUnsignedWord $4}
  | SIGNED WORD LBRACKET INT RBRACKET {Smv.TypeSignedWord $4}
  | LBRACE enumeration_type_body RBRACE {Smv.TypeEnum $2}
  | INT DOTDOT INT {Smv.TypeRange ($1, $3)}
  | ARRAY INT DOTDOT INT OF simple_type_specifier {Smv.TypeArray ($2, $4, $6)}
;

enumeration_type_body: enumeration_type_value COMMA enumeration_type_body {$1 :: $3}
  | enumeration_type_value {[$1]}
  | {[]}
;

enumeration_type_value: SYMBOL {Smv.EnumSym $1}
  | INT {Smv.EnumInt $1}
;

module_type_specifier: SYMBOL opt_parameter_list {Smv.ModType ($1, $2) }
  | PROCESS SYMBOL opt_parameter_list {Smv.ModProcessType ($2, $3)}
;

type_specifier: simple_type_specifier {TypeSimple $1}
  | module_type_specifier {TypeModule $1}
;

parameter_list: simple_expr COMMA parameter_list {$1 :: $3}
  | simple_expr {[$1]}
  | {[]}
;

opt_parameter_list: LPAREN parameter_list RPAREN {$2}
  | {[]}
;

simple_expr: basic_expr {$1}
;

next_expr: basic_expr {$1}
;

case_body: basic_expr COLON basic_expr SEMI case_body {Smv.Case ($1, $3) :: $5}
| { [] }
;

case_expr: CASE case_body ESAC {Smv.ExprCase $2}
;

var_list: complex_identifier COLON type_specifier SEMI var_list {Smv.VarDecl ($1, $3) :: $5 }
  | {[]}
;


ivar_list: complex_identifier COLON simple_type_specifier SEMI ivar_list {
				    Smv.IVarDecl ($1, $3) :: $5}
  | {[]}
;

frozenvar_list: complex_identifier COLON simple_type_specifier SEMI frozenvar_list {
				    Smv.FrozenVarDecl ($1, $3) :: $5}
  | {[]}
;

var_declaration: VAR var_list {Smv.ModVarDecl $2}
;

ivar_declaration: IVAR ivar_list {Smv.ModIVarDecl $2}
;

frozenvar_declaration: FROZENVAR frozenvar_list {Smv.ModFrozenVarDecl $2}
;

define_body: complex_identifier COLON_EQUAL next_expr SEMI define_body {
				Smv.DefineDecl ($1, $3) :: $5}
  | {[]}
;

define_declaration: DEFINE define_body {Smv.ModDefineDecl $2}
;

constants_body: complex_identifier COMMA constants_body {$1 :: $3}
  | complex_identifier {[$1]}
  | {[]}
;

constants_declaration: CONSTANTS constants_body {Smv.ModConstDecl $2}
;

assign: complex_identifier COLON_EQUAL simple_expr SEMI {Smv.AssignVar ($1, $3)}
  | INIT LPAREN complex_identifier RPAREN COLON_EQUAL simple_expr SEMI {Smv.AssignInit ($3, $6)}
  | NEXT LPAREN complex_identifier RPAREN COLON_EQUAL simple_expr SEMI {Smv.AssignNext ($3, $6)}
;

assign_list: assign assign_list {$1 :: $2}
  | {[]}
;

assign_constraint: ASSIGN assign_list {Smv.ModAssign $2}

opt_semi: SEMI {}
  | {}
;

trans_constraint: TRANS next_expr opt_semi {Smv.ModTrans $2}
;

init_constraint: INIT_STMT simple_expr opt_semi {Smv.ModInit $2}
;

invar_constraint: INVAR simple_expr opt_semi {Smv.ModInvar $2}
;

isa_declaration: ISA SYMBOL {Smv.ModIsaDecl $2}
;

ctl_specification: CTLSPEC ctl_expr SEMI {Smv.CtlCtlSpec $2}
  | SPEC ctl_expr opt_semi {Smv.CtlSpec $2}
  | CTLSPEC NAME SYMBOL COLON_EQUAL ctl_expr opt_semi {Smv.CtlCtlSpecName ($3, $5)}
  | SPEC NAME SYMBOL COLON_EQUAL ctl_expr opt_semi {Smv.CtlSpecName ($3, $5)}
;

ctl_expr: simple_expr {Smv.CtlExprExpr $1}
  | LPAREN ctl_expr RPAREN {$2}
  | NOT ctl_expr {Smv.CtlExprNot $2}
  | ctl_expr AND ctl_expr {Smv.CtlExprAnd ($1, $3)}
  | ctl_expr OR ctl_expr {Smv.CtlExprOr ($1, $3)}
  | ctl_expr XOR ctl_expr {Smv.CtlExprXor ($1, $3)}
  | ctl_expr XNOR ctl_expr {Smv.CtlExprXnor ($1, $3)}
  | ctl_expr IMPLIES ctl_expr {Smv.CtlExprImplies ($1, $3)}
  | ctl_expr EQUIV ctl_expr {Smv.CtlExprEquiv ($1, $3)}
  | EG ctl_expr {Smv.CtlExprExistsGlobal $2}
  | EX ctl_expr {Smv.CtlExprExistsNext $2}
  | EF ctl_expr {Smv.CtlExprExistsFinal $2}
  | AG ctl_expr {Smv.CtlExprForallGlobal $2}
  | AX ctl_expr {Smv.CtlExprForallNext $2}
  | AF ctl_expr {Smv.CtlExprForallFinal $2}
  | E LBRACKET ctl_expr U ctl_expr RBRACKET {Smv.CtlExprExistsUntil ($3, $5)}
  | A LBRACKET ctl_expr U ctl_expr RBRACKET {Smv.CtlExprForallUntil ($3, $5)}
;

invar_specification: INVARSPEC next_expr SEMI {Smv.InvarSpec $2}
  | INVARSPEC NAME SYMBOL COLON_EQUAL next_expr opt_semi {Smv.InvarSpecName ($3, $5)}
;

ltl_specification: LTLSPEC ltl_expr opt_semi {Smv.LtlSpec $2}
  | LTLSPEC NAME SYMBOL COLON_EQUAL ltl_expr opt_semi {Smv.LtlSpecName ($3, $5) }
;

ltl_expr: next_expr {Smv.LtlExprExpr $1}
  | LPAREN ltl_expr RPAREN {$2}
  | NOT ltl_expr {Smv.LtlExprNot $2}
  | ltl_expr AND ltl_expr {Smv.LtlExprAnd ($1, $3)}
  | ltl_expr OR ltl_expr {Smv.LtlExprOr ($1, $3)}
  | ltl_expr XOR ltl_expr {Smv.LtlExprXor ($1, $3)}
  | ltl_expr XNOR ltl_expr {Smv.LtlExprXnor ($1, $3)}
  | ltl_expr IMPLIES ltl_expr {Smv.LtlExprImplies ($1, $3)}
  | ltl_expr EQUIV ltl_expr {Smv.LtlExprEquiv ($1, $3)}
  | X ltl_expr {Smv.LtlExprNext $2 }
  | G ltl_expr {Smv.LtlExprGlobal $2}
  | F ltl_expr {Smv.LtlExprFinally $2}
  | ltl_expr U ltl_expr {Smv.LtlExprUntil ($1, $3)}
  | ltl_expr V ltl_expr {Smv.LtlExprReleases ($1, $3)}
  | Y ltl_expr {Smv.LtlExprPrevious $2}
  | Z ltl_expr {Smv.LtlExprNotPrevious $2}
  | H ltl_expr {Smv.LtlExprHistorically $2}
  | O ltl_expr {Smv.LtlExprOnce $2}
  | ltl_expr S ltl_expr {Smv.LtlExprSince ($1, $3)}
  | ltl_expr T ltl_expr {Smv.LtlExprTriggered ($1, $3) }
;

rtctl_specification: CTLSPEC rtctl_expr opt_semi {Smv.RtctlCtlSpec $2}
  | SPEC rtctl_expr opt_semi {Smv.RtctlSpec $2}
  | CTLSPEC NAME SYMBOL COLON_EQUAL rtctl_expr opt_semi {Smv.RtctlCtlSpecName ($3, $5)}
  | SPEC NAME SYMBOL COLON_EQUAL rtctl_expr opt_semi {Smv.RtctlSpecName ($3, $5)}
;

rtctl_expr: ctl_expr {Smv.RtctlCtlExpr $1}
  | EBF INT DOTDOT INT rtctl_expr {Smv.RtctlEBF ($2, $4, $5)}
  | ABF INT DOTDOT INT rtctl_expr {Smv.RtctlABF ($2, $4, $5)}
  | EBG INT DOTDOT INT rtctl_expr {Smv.RtctlEBG ($2, $4, $5)}
  | ABG INT DOTDOT INT rtctl_expr {Smv.RtctlABG ($2, $4, $5)}
  | A LBRACKET rtctl_expr BU INT DOTDOT INT rtctl_expr RBRACKET {
      Smv.RtctlA ($3, $5, $7, $8) }
  | E LBRACKET rtctl_expr BU INT DOTDOT INT rtctl_expr RBRACKET {
      Smv.RtctlE ($3, $5, $7, $8) }
;

compute_specification: COMPUTE compute_expr opt_semi {Smv.ComputeSpec $2}
  | COMPUTE NAME SYMBOL COLON_EQUAL compute_expr opt_semi {Smv.ComputeSpecName ($3, $5)}
;

compute_expr: MIN LBRACKET rtctl_expr COMMA rtctl_expr RBRACKET {Smv.ComputeMin ($3, $5)}
  | MAX LBRACKET rtctl_expr COMMA rtctl_expr RBRACKET {Smv.ComputeMax ($3, $5)}
;

fairness_constraint: FAIRNESS simple_expr opt_semi {Smv.FairnessFairness $2}
  | JUSTICE simple_expr opt_semi {Smv.FairnessJustice $2}
  | COMPASSION LPAREN simple_expr COMMA simple_expr RPAREN opt_semi {
		 Smv.FairnessCompassion ($3, $5) }
;

module_element: var_declaration {$1}
  | ivar_declaration {$1}
  | frozenvar_declaration {$1}
  | define_declaration {$1}
  | constants_declaration {$1}
  | assign_constraint {$1}
  | trans_constraint {$1}
  | init_constraint {$1}
  | invar_constraint {$1}
  | fairness_constraint {Smv.ModFairness $1}
  | ctl_specification {Smv.ModCtlSpec $1}
  | rtctl_specification {Smv.ModRtctlSpec $1}
  | invar_specification {Smv.ModInvarSpec $1}
  | ltl_specification {Smv.ModLtlSpec $1}
  | compute_specification {Smv.ModComputeSpec $1}
  | isa_declaration {$1}
;

module_parameters: SYMBOL COMMA module_parameters {$1 :: $3}
  | SYMBOL { [$1] }
;

opt_module_parameters: LPAREN module_parameters RPAREN {$2}
  | {[]}
;

module_body: module_element module_body {$1 :: $2}
  | module_element {[$1]}
;

opt_module_body: module_body { $1 }
  | {[]}
;

smv_module: MODULE SYMBOL opt_module_parameters opt_module_body EOF {
		   Smv.Module ($2, $3, $4) }
;
