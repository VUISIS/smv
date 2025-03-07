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

and add_unary_op buffer op expr =
  Buffer.add_string buffer op;
  Buffer.add_char buffer '(';
  add_basic_expr buffer expr;
  Buffer.add_char buffer ')'

and add_binary_op buffer op x y =
  Buffer.add_char buffer '(';
  add_basic_expr buffer x;
  Buffer.add_char buffer ')';
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer op;
  Buffer.add_char buffer ' ';
  Buffer.add_char buffer '(';
  add_basic_expr buffer y;
  Buffer.add_char buffer ')'
  
and add_basic_expr buffer expr =
  match expr with
  | ExprConst const_val -> add_const buffer const_val
  | ExprVar id -> add_id buffer id
  | ExprAbs expr -> add_unary_func buffer "abs" expr
  | ExprMax (x,y) -> add_binary_func buffer "max" x y
  | ExprMin (x,y) -> add_binary_func buffer "min" x y
  | ExprNot expr -> add_unary_op buffer "!" expr
  | ExprAnd (x,y) -> add_binary_op buffer "&" x y
  | ExprOr (x,y) -> add_binary_op buffer "|" x y
  | ExprXor (x,y) -> add_binary_op buffer "xor" x y
  | ExprXnor (x,y) -> add_binary_op buffer "xnor" x y
  | ExprImplies (x,y) -> add_binary_op buffer "->" x y
  | ExprEquiv (x,y) -> add_binary_op buffer "<->" x y
  | ExprEqual (x,y) -> add_binary_op buffer "=" x y
  | ExprNotEqual (x,y) -> add_binary_op buffer "!=" x y
  | ExprLess (x,y) -> add_binary_op buffer "<" x y
  | ExprLessEqual (x,y) -> add_binary_op buffer "<=" x y
  | ExprGreater (x,y) -> add_binary_op buffer ">" x y
  | ExprGreaterEqual (x,y) -> add_binary_op buffer ">=" x y
  | ExprNeg expr -> add_unary_op buffer "-" expr
  | ExprPlus (x,y) -> add_binary_op buffer "+" x y
  | ExprMinus (x,y) -> add_binary_op buffer "-" x y
  | ExprTimes (x,y) -> add_binary_op buffer "*" x y
  | ExprDiv (x,y) -> add_binary_op buffer "/" x y
  | ExprMod (x,y) -> add_binary_op buffer "mod" x y
  | ExprShiftLeft (x,y) -> add_binary_op buffer "<<" x y
  | ExprShiftRight (x,y) -> add_binary_op buffer ">>" x y
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
  | ExprConcat (x,y) -> add_binary_op buffer "::" x y
  | ExprWord1 expr -> add_unary_func buffer "word1" expr
  | ExprBool expr -> add_unary_func buffer "bool" expr
  | ExprToInt expr -> add_unary_func buffer "toint" expr
  | ExprSigned expr -> add_unary_func buffer "signed" expr
  | ExprUnsigned expr -> add_unary_func buffer "unsigned" expr
  | ExprSizeof expr -> add_unary_func buffer "sizeof" expr
  | ExprExtend (x,y) -> add_binary_func buffer "extend" x y
  | ExprResize (x,y) -> add_binary_func buffer "resize" x y
  | ExprUnion (x,y) -> add_binary_op buffer "union" x y
  | ExprCount exprs ->
     (Buffer.add_string buffer "count";
      add_bounded_list buffer add_basic_expr ", " "(" ")" exprs)
  | ExprSet exprs ->
     add_bounded_list buffer add_basic_expr ", " "{" "}" exprs
  | ExprIn (x, y) -> add_binary_op buffer "in" x y
  | ExprTernary (x,y,z) ->
     (add_basic_expr buffer x;
      Buffer.add_string buffer " ? (";
      add_basic_expr buffer y;
      Buffer.add_string buffer ") : (";
      add_basic_expr buffer z;
      Buffer.add_char buffer ')')
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
  
