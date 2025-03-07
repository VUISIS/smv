open Smv;;

let rec add_list buffer item_add sep items =
  match items with
  | (a :: b :: rest) -> (item_add buffer a;
                       Buffer.add_string buffer sep;
                       add_list buffer item_add sep (b :: rest))
  | a :: [] -> item_add buffer a
  | [] -> ()

let add_bounded_list buffer item_add sep left right items =
  Buffer.add_string buffer left;
  add_list buffer item_add sep items;
  Buffer.add_string buffer right


let rec add_program buffer (Program module_list) =
  List.iter (add_module buffer) module_list

and add_module buffer (Module (name, params, elems)) =
  let write_params = function
    | (_x::_xs) -> (Buffer.add_string buffer " (";
               Buffer.add_string buffer (String.concat "," params);
               Buffer.add_string buffer ")")
    | _ -> () in
  Buffer.add_string buffer "MODULE ";
  Buffer.add_string buffer name;
  write_params params;
  Buffer.add_char buffer '\n';
  List.iter (add_module_elem buffer) elems

and add_module_elem buffer = function
  | ModVarDecl var_decls ->
     (Buffer.add_string buffer "  VAR\n";
      List.iter (add_var_decl buffer) var_decls)
  | ModIVarDecl ivar_decls ->
     (Buffer.add_string buffer "  IVAR\n";
      List.iter (add_ivar_decl buffer) ivar_decls)
  | ModDefineDecl def_decls ->
     (Buffer.add_string buffer "  DEFINE\n";
      List.iter (add_def_decl buffer) def_decls)
  | ModConstDecl const_decls ->
     (Buffer.add_string buffer "  CONSTANTS\n";
      Buffer.add_string buffer "    ";
      add_list buffer add_id ", " const_decls)
  | ModAssign assigns ->
     (Buffer.add_string buffer "  ASSIGN\n";
      List.iter (add_assign buffer) assigns)
  | ModTrans trans ->
     (Buffer.add_string buffer "  TRANS ";
      add_basic_expr buffer trans;
      Buffer.add_string buffer ";\n")
  | ModInit init ->
     (Buffer.add_string buffer "  INIT ";
      add_basic_expr buffer init;
      Buffer.add_string buffer ";\n")
  | ModInvar invar ->
     (Buffer.add_string buffer "  INVAR ";
      add_basic_expr buffer invar;
      Buffer.add_string buffer ";\n")
  | ModFairness fairness ->
     add_fairness buffer fairness
  | ModCtlSpec ctl_spec ->
     add_ctl_spec buffer ctl_spec
  | ModRtctlSpec rtctl_spec ->
     add_rtctl_spec buffer rtctl_spec
  | ModInvarSpec invar_spec ->
     add_invar_spec buffer invar_spec
  | ModLtlSpec ltl_spec ->
     add_ltl_spec buffer ltl_spec
  | ModComputeSpec compute_spec ->
     add_compute_spec buffer compute_spec
  | ModIsaDecl s ->
     (Buffer.add_string buffer "  ISA ";
      Buffer.add_string buffer s;
      Buffer.add_string buffer "\n")
  | _ -> ()

and add_var_decl buffer (VarDecl (id,var_type)) =
  Buffer.add_string buffer "    ";
  add_id buffer id;
  Buffer.add_string buffer " : ";
  add_type buffer var_type;
  Buffer.add_string buffer ";\n"

and add_ivar_decl buffer (IVarDecl (id, var_type)) =
  Buffer.add_string buffer "    ";
  add_id buffer id;
  Buffer.add_string buffer " : ";
  add_simple_type buffer var_type;
  Buffer.add_string buffer ";\n"

and add_def_decl buffer (DefineDecl (id, expr)) =
  Buffer.add_string buffer "    ";
  add_id buffer id;
  Buffer.add_string buffer " := ";
  add_basic_expr buffer expr;
  Buffer.add_string buffer ";\n"

and add_assign buffer assign =
  Buffer.add_string buffer "    ";
  match assign with
  | AssignVar (id, expr) ->
     (add_id buffer id;
      Buffer.add_string buffer " :=\n      ";
      add_basic_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | AssignInit (id, expr) ->
     (Buffer.add_string buffer "init(";
      add_id buffer id;
      Buffer.add_string buffer ") :=\n      ";
      add_basic_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | AssignNext (id, expr) ->
     (Buffer.add_string buffer "next(";
      add_id buffer id;
      Buffer.add_string buffer ") :=\n      ";
      add_basic_expr buffer expr;
      Buffer.add_string buffer ";\n")

and add_fairness buffer fairness =
  match fairness with
  | FairnessFairness expr ->
     (Buffer.add_string buffer "  FAIRNESS ";
      add_basic_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | FairnessJustice expr ->
     (Buffer.add_string buffer "  JUSTICE ";
      add_basic_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | FairnessCompassion (x,y) ->
     (Buffer.add_string buffer "  COMPASSION (";
      add_basic_expr buffer x;
      Buffer.add_string buffer ", ";
      add_basic_expr buffer y;
      Buffer.add_string buffer ");\n")

and add_ctl_spec buffer ctl_spec =
  match ctl_spec with
  | CtlCtlSpec expr ->
     (Buffer.add_string buffer "  CTLSPEC ";
      add_ctl_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | CtlSpec expr ->
     (Buffer.add_string buffer "  SPEC ";
      add_ctl_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | CtlCtlSpecName (name,expr) ->
     (Buffer.add_string buffer "  CTLSPEC NAME ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " := ";
      add_ctl_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | CtlSpecName (name,expr) ->
     (Buffer.add_string buffer "  SPEC NAME ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " := ";
      add_ctl_expr buffer expr;
      Buffer.add_string buffer ";\n")

and add_ctl_unary_op buffer op prio expr =
  let expr_prio = ctlexpr_priority expr in
  Buffer.add_string buffer op;
  if expr_prio > prio then
    (Buffer.add_char buffer '(';
     add_ctl_expr buffer expr;
     Buffer.add_char buffer ')')
  else
     add_ctl_expr buffer expr;

and add_ctl_binary_op buffer op prio x y =
  let left_prio = ctlexpr_priority x in
  let right_prio = ctlexpr_priority y in
  if left_prio > prio then
    (Buffer.add_char buffer '(';
     add_ctl_expr buffer x;
     Buffer.add_char buffer ')')
  else
     add_ctl_expr buffer x;
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer op;
  Buffer.add_char buffer ' ';
  if right_prio > prio then
    (Buffer.add_char buffer '(';
     add_ctl_expr buffer y;
     Buffer.add_char buffer ')')
  else
     add_ctl_expr buffer y  

and add_ctl_expr buffer expr =
  match expr with
  | CtlExprExpr expr ->
     (Buffer.add_char buffer '(';
      add_basic_expr buffer expr;
      Buffer.add_char buffer ')')
  | CtlExprNot not_expr ->
     add_ctl_unary_op buffer "!" (ctlexpr_priority expr) not_expr
  | CtlExprAnd (x,y) ->
     add_ctl_binary_op buffer "&" (ctlexpr_priority expr) x y
  | CtlExprOr (x,y) ->
     add_ctl_binary_op buffer "|" (ctlexpr_priority expr) x y
  | CtlExprXor (x,y) ->
     add_ctl_binary_op buffer "xor" (ctlexpr_priority expr) x y
  | CtlExprXnor (x,y) ->
     add_ctl_binary_op buffer "xnor" (ctlexpr_priority expr) x y
  | CtlExprImplies (x,y) ->
     add_ctl_binary_op buffer "->" (ctlexpr_priority expr) x y
  | CtlExprEquiv (x,y) ->
     add_ctl_binary_op buffer "<->" (ctlexpr_priority expr) x y
  | CtlExprExistsGlobal x ->
     add_ctl_unary_op buffer "EG" (ctlexpr_priority expr) x
  | CtlExprExistsNext x ->
     add_ctl_unary_op buffer "EX" (ctlexpr_priority expr) x
  | CtlExprExistsFinal x ->
     add_ctl_unary_op buffer "EF" (ctlexpr_priority expr) x
  | CtlExprForallGlobal x ->
     add_ctl_unary_op buffer "AG" (ctlexpr_priority expr) x
  | CtlExprForallNext x ->
     add_ctl_unary_op buffer "AX" (ctlexpr_priority expr) x
  | CtlExprForallFinal x ->
     add_ctl_unary_op buffer "AF" (ctlexpr_priority expr) x
  | CtlExprExistsUntil (x,y) ->
     (Buffer.add_string buffer "A [";
      add_ctl_expr buffer x;
      Buffer.add_string buffer " U ";
      add_ctl_expr buffer y;
      Buffer.add_char buffer ']')
  | CtlExprForallUntil (x,y) ->
     (Buffer.add_string buffer "E [";
      add_ctl_expr buffer x;
      Buffer.add_string buffer " U ";
      add_ctl_expr buffer y;
      Buffer.add_char buffer ']')

and add_rtctl_spec buffer rtctl_spec =
  match rtctl_spec with
  | RtctlCtlSpec expr ->
     (Buffer.add_string buffer "  CTLSPEC ";
      add_rtctl_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | RtctlSpec expr ->
     (Buffer.add_string buffer "  SPEC ";
      add_rtctl_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | RtctlCtlSpecName (name,expr) ->
     (Buffer.add_string buffer "  CTLSPEC NAME ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " := ";
      add_rtctl_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | RtctlSpecName (name,expr) ->
     (Buffer.add_string buffer "  SPEC NAME ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " := ";
      add_rtctl_expr buffer expr;
      Buffer.add_string buffer ";\n")

and add_rtctl_expr buffer rtctl_expr =
  match rtctl_expr with
  | RtctlCtlExpr expr ->
     (Buffer.add_char buffer '(';
      add_ctl_expr buffer expr;
      Buffer.add_char buffer ')')
  | RtctlEBF (i,j,expr) ->
     (Buffer.add_string buffer "EBF ";
      Buffer.add_string buffer (string_of_int i);
      Buffer.add_string buffer " .. ";
      Buffer.add_string buffer (string_of_int j);
      Buffer.add_char buffer ' ';
      add_rtctl_expr buffer expr)
  | RtctlABF (i,j,expr) ->
     (Buffer.add_string buffer "ABF ";
      Buffer.add_string buffer (string_of_int i);
      Buffer.add_string buffer " .. ";
      Buffer.add_string buffer (string_of_int j);
      Buffer.add_char buffer ' ';
      add_rtctl_expr buffer expr)
  | RtctlEBG (i,j,expr) ->
     (Buffer.add_string buffer "EBG ";
      Buffer.add_string buffer (string_of_int i);
      Buffer.add_string buffer " .. ";
      Buffer.add_string buffer (string_of_int j);
      Buffer.add_char buffer ' ';
      add_rtctl_expr buffer expr)
  | RtctlABG (i,j,expr) ->
     (Buffer.add_string buffer "ABG ";
      Buffer.add_string buffer (string_of_int i);
      Buffer.add_string buffer " .. ";
      Buffer.add_string buffer (string_of_int j);
      Buffer.add_char buffer ' ';
      add_rtctl_expr buffer expr)
  | RtctlA (expr1, i, j, expr2) ->
     (Buffer.add_string buffer "A [";
      add_rtctl_expr buffer expr1;
      Buffer.add_string buffer " BU ";
      Buffer.add_string buffer (string_of_int i);
      Buffer.add_string buffer " .. ";
      Buffer.add_string buffer (string_of_int j);
      Buffer.add_char buffer ' ';
      add_rtctl_expr buffer expr2;
      Buffer.add_char buffer ']')
  | RtctlE (expr1, i, j, expr2) ->
     (Buffer.add_string buffer "E [";
      add_rtctl_expr buffer expr1;
      Buffer.add_string buffer " BU ";
      Buffer.add_string buffer (string_of_int i);
      Buffer.add_string buffer " .. ";
      Buffer.add_string buffer (string_of_int j);
      Buffer.add_char buffer ' ';
      add_rtctl_expr buffer expr2;
      Buffer.add_char buffer ']')

and add_invar_spec buffer invar_spec =
  match invar_spec with
  | InvarSpec expr ->
     (Buffer.add_string buffer "  INVARSPEC ";
      add_basic_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | InvarSpecName (name, expr) ->
     (Buffer.add_string buffer "  INVARSPEC NAME ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " := ";
      add_basic_expr buffer expr;
      Buffer.add_string buffer ";\n")

and add_ltl_spec buffer ltl_spec =
  match ltl_spec with
  | LtlSpec expr ->
     (Buffer.add_string buffer "  LTLSPEC ";
      add_ltl_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | LtlSpecName (name,expr) ->
     (Buffer.add_string buffer "  LTLSPEC NAME ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " := ";
      add_ltl_expr buffer expr;
      Buffer.add_string buffer ";\n")

and add_ltl_unary_op buffer op prio expr =
  let expr_prio = ltlexpr_priority expr in
  Buffer.add_string buffer op;
  if expr_prio > prio then
    (Buffer.add_char buffer '(';
     add_ltl_expr buffer expr;
     Buffer.add_char buffer ')')
  else
     add_ltl_expr buffer expr;

and add_ltl_binary_op buffer op prio x y =
  let left_prio = ltlexpr_priority x in
  let right_prio = ltlexpr_priority y in
  if left_prio > prio then
    (Buffer.add_char buffer '(';
     add_ltl_expr buffer x;
     Buffer.add_char buffer ')')
  else
     add_ltl_expr buffer x;
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer op;
  Buffer.add_char buffer ' ';
  if right_prio > prio then
    (Buffer.add_char buffer '(';
     add_ltl_expr buffer y;
     Buffer.add_char buffer ')')
  else
     add_ltl_expr buffer y  

and add_ltl_expr buffer expr =
  match expr with
  | LtlExprExpr expr ->
     (Buffer.add_char buffer '(';
      add_basic_expr buffer expr;
      Buffer.add_char buffer ')')
  | LtlExprNot not_expr ->
     add_ltl_unary_op buffer "!" (ltlexpr_priority expr) not_expr
  | LtlExprAnd (x,y) ->
     add_ltl_binary_op buffer "&" (ltlexpr_priority expr) x y
  | LtlExprOr (x,y) ->
     add_ltl_binary_op buffer "|" (ltlexpr_priority expr) x y
  | LtlExprXor (x,y) ->
     add_ltl_binary_op buffer "xor" (ltlexpr_priority expr) x y
  | LtlExprXnor (x,y) ->
     add_ltl_binary_op buffer "xnor" (ltlexpr_priority expr) x y
  | LtlExprImplies (x,y) ->
     add_ltl_binary_op buffer "->" (ltlexpr_priority expr) x y
  | LtlExprEquiv (x,y) ->
     add_ltl_binary_op buffer "<->" (ltlexpr_priority expr) x y
  | LtlExprNext x ->
     add_ltl_unary_op buffer "X" (ltlexpr_priority expr) x
  | LtlExprGlobal x ->
     add_ltl_unary_op buffer "G" (ltlexpr_priority expr) x
  | LtlExprFinally x ->
     add_ltl_unary_op buffer "F" (ltlexpr_priority expr) x
  | LtlExprUntil (x, y) ->
     add_ltl_binary_op buffer "U" (ltlexpr_priority expr) x y
  | LtlExprReleases (x, y) ->
     add_ltl_binary_op buffer "R" (ltlexpr_priority expr) x y
  | LtlExprPrevious x ->
     add_ltl_unary_op buffer "Y" (ltlexpr_priority expr) x
  | LtlExprNotPrevious x ->
     add_ltl_unary_op buffer "Z" (ltlexpr_priority expr) x
  | LtlExprHistorically x ->
     add_ltl_unary_op buffer "H" (ltlexpr_priority expr) x
  | LtlExprOnce x ->
     add_ltl_unary_op buffer "O" (ltlexpr_priority expr) x
  | LtlExprSince (x, y) ->
     add_ltl_binary_op buffer "S" (ltlexpr_priority expr) x y
  | LtlExprTriggered (x, y) ->
     add_ltl_binary_op buffer "T" (ltlexpr_priority expr) x y

and add_compute_spec buffer compute_spec =
  match compute_spec with
  | ComputeSpec expr ->
     (Buffer.add_string buffer "  COMPUTE ";
      add_compute_expr buffer expr;
      Buffer.add_string buffer ";\n")
  | ComputeSpecName (name, expr) ->
     (Buffer.add_string buffer "  COMPUTE NAME ";
      Buffer.add_string buffer name;
      Buffer.add_string buffer " := ";
      add_compute_expr buffer expr;
      Buffer.add_string buffer ";\n")

and add_compute_expr buffer compute_expr =
  match compute_expr with
  | ComputeMin (x, y) ->
     (Buffer.add_string buffer "MIN [";
      add_rtctl_expr buffer x;
      Buffer.add_string buffer ", ";
      add_rtctl_expr buffer y;
      Buffer.add_char buffer ']')
  | ComputeMax (x, y) ->
     (Buffer.add_string buffer "MAX [";
      add_rtctl_expr buffer x;
      Buffer.add_string buffer ", ";
      add_rtctl_expr buffer y;
      Buffer.add_char buffer ']')
and add_id buffer id =
  match id with
  | IdSym str -> Buffer.add_string buffer str
  | IdDot (prev_id,str) -> (add_id buffer prev_id; Buffer.add_string buffer str)
  | IdRef (prev_id,expr) -> (add_id buffer prev_id;
                           Buffer.add_char buffer '[';
                           add_basic_expr buffer expr;
                           Buffer.add_char buffer ']')
  | IdSelf -> Buffer.add_string buffer "self"

and add_type buffer some_type =
  match some_type with
  | TypeSimple simple_type -> add_simple_type buffer simple_type
  | TypeModule module_type -> add_module_type buffer module_type

and add_enum buffer enum_val =
  match enum_val with
  | EnumInt i -> Buffer.add_string buffer (string_of_int i)
  | EnumSym s -> Buffer.add_string buffer s

and add_simple_type buffer simple_type =
  match simple_type with
  | TypeBoolean -> Buffer.add_string buffer "boolean"
  | TypeWord i -> (Buffer.add_string buffer "word[";
                   Buffer.add_string buffer (string_of_int i);
                   Buffer.add_char buffer ']') 
  | TypeUnsignedWord i -> (Buffer.add_string buffer "unsigned word[";
                   Buffer.add_string buffer (string_of_int i);
                   Buffer.add_char buffer ']') 
  | TypeSignedWord i -> (Buffer.add_string buffer "signed word[";
                   Buffer.add_string buffer (string_of_int i);
                   Buffer.add_char buffer ']')
  |  TypeEnum enums -> (Buffer.add_string buffer " {";
                        add_list buffer add_enum ", " enums;
                        Buffer.add_char buffer '}')
  | TypeRange (low,high) -> (Buffer.add_string buffer (string_of_int low);
                             Buffer.add_string buffer " .. ";
                             Buffer.add_string buffer (string_of_int high))
  | TypeArray (low, high, arr_type) -> (Buffer.add_string buffer "array ";
                                        Buffer.add_string buffer (string_of_int low);
                                        Buffer.add_string buffer " .. ";
                                        Buffer.add_string buffer (string_of_int high);
                                        Buffer.add_string buffer " of ";
                                        add_simple_type buffer arr_type)
and add_module_type buffer module_type =
  match module_type with
  | ModType (name, exprs) ->
     (Buffer.add_string buffer name;
      add_bounded_list buffer add_basic_expr ", " "(" ")" exprs)
  | ModProcessType (name, exprs) ->
     (Buffer.add_string buffer "process ";
      Buffer.add_string buffer name;
      add_bounded_list buffer add_basic_expr ", " "(" ")" exprs)

and add_const buffer const_val =
  match const_val with
  | ConstBool true -> Buffer.add_string buffer "TRUE"
  | ConstBool false -> Buffer.add_string buffer "FALSE"
  | ConstInt i -> Buffer.add_string buffer (string_of_int i)
  | ConstWord i -> Buffer.add_string buffer (string_of_int i)
  | ConstRange (low,high) -> (Buffer.add_string buffer (string_of_int low);
                              Buffer.add_string buffer " .. ";
                              Buffer.add_string buffer (string_of_int high))
and add_unary_func buffer func expr =
  Buffer.add_string buffer func;
  Buffer.add_char buffer '(';
  add_basic_expr buffer expr;
  Buffer.add_char buffer ')'

and add_binary_func buffer func x y =
  Buffer.add_string buffer func;
  Buffer.add_char buffer '(';
  add_basic_expr buffer x;
  Buffer.add_string buffer ", ";
  add_basic_expr buffer y;
  Buffer.add_char buffer ')'

and add_unary_op buffer op prio expr =
  let expr_prio = expr_priority expr in
  Buffer.add_string buffer op;
  if expr_prio > prio then
    (Buffer.add_char buffer '(';
     add_basic_expr buffer expr;
     Buffer.add_char buffer ')')
  else
     add_basic_expr buffer expr;

and add_binary_op buffer op prio x y =
  let left_prio = expr_priority x in
  let right_prio = expr_priority y in
  if left_prio > prio then
    (Buffer.add_char buffer '(';
     add_basic_expr buffer x;
     Buffer.add_char buffer ')')
  else
     add_basic_expr buffer x;
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer op;
  Buffer.add_char buffer ' ';
  if right_prio > prio then
    (Buffer.add_char buffer '(';
     add_basic_expr buffer y;
     Buffer.add_char buffer ')')
  else
     add_basic_expr buffer y  
and add_basic_expr buffer expr =
  match expr with
  | ExprConst const_val -> add_const buffer const_val
  | ExprVar id -> add_id buffer id
  | ExprAbs expr -> add_unary_func buffer "abs" expr
  | ExprMax (x,y) -> add_binary_func buffer "max" x y
  | ExprMin (x,y) -> add_binary_func buffer "min" x y
  | ExprNot not_expr -> add_unary_op buffer "!" (expr_priority expr) not_expr
  | ExprAnd (x,y) -> add_binary_op buffer "&" (expr_priority expr) x y
  | ExprOr (x,y) -> add_binary_op buffer "|" (expr_priority expr) x y
  | ExprXor (x,y) -> add_binary_op buffer "xor" (expr_priority expr) x y
  | ExprXnor (x,y) -> add_binary_op buffer "xnor" (expr_priority expr) x y
  | ExprImplies (x,y) -> add_binary_op buffer "->" (expr_priority expr) x y
  | ExprEquiv (x,y) -> add_binary_op buffer "<->" (expr_priority expr) x y
  | ExprEqual (x,y) -> add_binary_op buffer "=" (expr_priority expr) x y
  | ExprNotEqual (x,y) -> add_binary_op buffer "!=" (expr_priority expr) x y
  | ExprLess (x,y) -> add_binary_op buffer "<" (expr_priority expr) x y
  | ExprLessEqual (x,y) -> add_binary_op buffer "<=" (expr_priority expr) x y
  | ExprGreater (x,y) -> add_binary_op buffer ">" (expr_priority expr) x y
  | ExprGreaterEqual (x,y) -> add_binary_op buffer ">=" (expr_priority expr) x y
  | ExprNeg neg_expr -> add_unary_op buffer "-" (expr_priority expr) neg_expr
  | ExprPlus (x,y) -> add_binary_op buffer "+" (expr_priority expr) x y
  | ExprMinus (x,y) -> add_binary_op buffer "-" (expr_priority expr) x y
  | ExprTimes (x,y) -> add_binary_op buffer "*" (expr_priority expr) x y
  | ExprDiv (x,y) -> add_binary_op buffer "/" (expr_priority expr) x y
  | ExprMod (x,y) -> add_binary_op buffer "mod" (expr_priority expr) x y
  | ExprShiftLeft (x,y) -> add_binary_op buffer "<<" (expr_priority expr) x y
  | ExprShiftRight (x,y) -> add_binary_op buffer ">>" (expr_priority expr) x y
  | ExprArrayRef (x,y) ->
     (add_basic_expr buffer x;
      Buffer.add_char buffer '[';
      add_basic_expr buffer y;
      Buffer.add_char buffer ']')
  | ExprBitSel (x,y,z) ->
     (add_basic_expr buffer x;
      Buffer.add_char buffer '[';
      Buffer.add_string buffer (string_of_int y);
      Buffer.add_string buffer " : ";
      Buffer.add_string buffer (string_of_int z);
      Buffer.add_char buffer ']')
  | ExprConcat (x,y) -> add_binary_op buffer "::" (expr_priority expr) x y
  | ExprWord1 expr -> add_unary_func buffer "word1" expr
  | ExprBool expr -> add_unary_func buffer "bool" expr
  | ExprToInt expr -> add_unary_func buffer "toint" expr
  | ExprSigned expr -> add_unary_func buffer "signed" expr
  | ExprUnsigned expr -> add_unary_func buffer "unsigned" expr
  | ExprSizeof expr -> add_unary_func buffer "sizeof" expr
  | ExprExtend (x,y) -> add_binary_func buffer "extend" x y
  | ExprResize (x,y) -> add_binary_func buffer "resize" x y
  | ExprUnion (x,y) -> add_binary_op buffer "union" (expr_priority expr) x y
  | ExprCount exprs ->
     (Buffer.add_string buffer "count";
      add_bounded_list buffer add_basic_expr ", " "(" ")" exprs)
  | ExprSet exprs ->
     add_bounded_list buffer add_basic_expr ", " "{" "}" exprs
  | ExprIn (x, y) -> add_binary_op buffer "in" (expr_priority expr) x y
  | ExprTernary (x,y,z) ->
     (let prio = expr_priority (ExprTernary (x,y,z)) in
      let x_prio = expr_priority x in
      let y_prio = expr_priority y in
      let z_prio = expr_priority z in
      if x_prio > prio then
        (Buffer.add_char buffer '(';
         add_basic_expr buffer x;
         Buffer.add_char buffer ')')
      else
         add_basic_expr buffer x;        
      Buffer.add_string buffer " ? ";
      if y_prio > prio then
        (Buffer.add_char buffer '(';
         add_basic_expr buffer y;
         Buffer.add_char buffer ')')
      else
         add_basic_expr buffer y;        
      Buffer.add_string buffer " : ";
      if z_prio > prio then
        (Buffer.add_char buffer '(';
         add_basic_expr buffer z;
         Buffer.add_char buffer ')')
      else
         add_basic_expr buffer z)
  | ExprCase cases ->
     (Buffer.add_string buffer "case\n";
      List.iter (add_case buffer) cases;
      Buffer.add_string buffer "      esac")
  | ExprNext expr ->
     (Buffer.add_string buffer "next (";
      add_basic_expr buffer expr;
      Buffer.add_char buffer ')')

and add_case buffer (Case (match_expr, result_expr)) =
  Buffer.add_string buffer "        ";
  add_basic_expr buffer match_expr;
  Buffer.add_string buffer " : ";
  add_basic_expr buffer result_expr;
  Buffer.add_string buffer ";\n"

let to_string add_expr expr =
  let buff = Buffer.create 1024 in
  add_expr buff expr;
  Buffer.contents buff

let print_test () =
  let parsed = Smv_io.parse_smv "../c_to_nusmv/cli_untar.smv" in
  Printf.printf "%s\n" (to_string add_program parsed)

let print_file filename =
  let parsed = Smv_io.parse_smv "../c_to_nusmv/cli_untar_stripped.smv" in
  Out_channel.with_open_text filename
    (fun f ->
      Printf.fprintf f "%s\n" (to_string add_program parsed))
  
