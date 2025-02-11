type array_elem_type = Array_Int | Array_Bool | Array_Range of int * int
                                                               
type decl_type =
  | Bool_Decl of string
  | Int_Decl of string
  | Int_Range_Decl of string * int * int
  | Enum_Decl of string * string list
  | Array_Decl of string * int * int * array_elem_type


type expr_type =
  | Expr_Bool of bool
  | Expr_Int of int
  | Expr_Var of string
  | Expr_Array of expr_type list
  | Expr_Array_Ref of expr_type * expr_type
  | Expr_Add of expr_type * expr_type
  | Expr_Sub of expr_type * expr_type
  | Expr_Mul of expr_type * expr_type
  | Expr_Div of expr_type * expr_type
  | Expr_Mod of expr_type * expr_type
  | Expr_Neg of expr_type
  | Expr_And of expr_type * expr_type
  | Expr_Or of expr_type * expr_type
  | Expr_Not of expr_type
  | Expr_Xor of expr_type * expr_type
  | Expr_Eq of expr_type * expr_type
  | Expr_Ne of expr_type * expr_type
  | Expr_Lt of expr_type * expr_type
  | Expr_Gt of expr_type * expr_type
  | Expr_Le of expr_type * expr_type
  | Expr_Ge of expr_type * expr_type
  | Expr_Func of string * expr_type list
                   
and lvalue_type = LVal_Var of string | LVal_ARef of string * expr_type                   
and definition_type = Definition of lvalue_type * expr_type
                                    
type case_result_type = Single of expr_type | Choose of expr_type list
type case_expr = Case of expr_type * case_result_type
    
type stmt_value_type = Val_Case of case_expr list | Val_Expr of expr_type
                                                
type stmt_type =
  | Init of lvalue_type * stmt_value_type
  | Next of lvalue_type * stmt_value_type

type ltl_type =
  | Invarspec of expr_type
  | Ltl of string * expr_type

type program_type = Program of string * decl_type list * definition_type list *
                               stmt_type list * ltl_type list

let string_of_elem_type = function
  | Array_Int -> "integer"
  | Array_Bool -> "boolean"
  | Array_Range (start, finish) -> (string_of_int start) ^ " .. " ^ (string_of_int finish)

let print_decl out indent decl =
  match decl with
  | Bool_Decl sym -> Printf.fprintf out "%s%s : boolean;\n" indent sym
  | Int_Decl sym -> Printf.fprintf out "%s%s : -2147483647.. 2147483647;\n" indent sym
  | Int_Range_Decl (sym,start_val,end_val) ->
    Printf.fprintf out "%s%s : %d .. %d;\n" indent sym start_val end_val
  | Enum_Decl (sym,strs) -> Printf.fprintf out "%s%s : { %s };\n" indent sym
                              (String.concat ", " strs)
  | Array_Decl (sym, lower, upper, arr_type) ->
    Printf.fprintf out "%s%s : array %d .. %d of %s;\n" indent sym lower upper
      (string_of_elem_type arr_type)
      
let rec string_of_expr wrap expr =
  let needs_wrap = function
    | Expr_Bool _ -> false
    | Expr_Int _ -> false
    | Expr_Var _ -> false
    | Expr_Array_Ref _ -> false
    | _ -> true in
  let wrap_left = if wrap then "(" else "" in
  let wrap_right = if wrap then ")" else "" in
  let string_of_unary op ex =
    Printf.sprintf "%s%s%s%s" wrap_left
      op
      (string_of_expr (needs_wrap ex) ex)
      wrap_right in
  let string_of_binary op left right =
    Printf.sprintf "%s%s%s%s%s" wrap_left
      (string_of_expr true left) op (string_of_expr true right)
      wrap_right in
  let string_of_list exprs =
    match exprs with
    | [] -> ""
    | _ ->
      let expr_strs = List.map (fun ex -> string_of_expr (needs_wrap ex) ex)
          exprs in
      String.concat "," expr_strs in
  match expr with
  | Expr_Bool true -> "TRUE"
  | Expr_Bool false -> "FALSE"
  | Expr_Int i -> Printf.sprintf "%d" i
  | Expr_Var sym -> Printf.sprintf "%s" sym
  | Expr_Array expr_list -> "[" ^ (string_of_list expr_list) ^ "]"
  | Expr_Array_Ref (ex1,ex2) -> (string_of_expr false ex1) ^ "[" ^
                                (string_of_expr false ex2) ^ "]"
  | Expr_Add (left,right) -> string_of_binary " + " left right
  | Expr_Sub (left,right) -> string_of_binary " - " left right
  | Expr_Mul (left,right) -> string_of_binary " * " left right
  | Expr_Div (left,right) -> string_of_binary " / " left right
  | Expr_Mod (left,right) -> string_of_binary " mod " left right
  | Expr_Neg ex -> string_of_unary "-" ex
  | Expr_Not ex -> string_of_unary "!" ex
  | Expr_And (left,right) -> string_of_binary " & " left right
  | Expr_Or (left,right) -> string_of_binary " | " left right
  | Expr_Xor (left,right) -> string_of_binary " xor " left right
  | Expr_Eq (left,right) -> string_of_binary " = " left right
  | Expr_Ne (left,right) -> string_of_binary " != " left right
  | Expr_Lt (left,right) -> string_of_binary " < " left right
  | Expr_Le (left,right) -> string_of_binary " <= " left right
  | Expr_Gt (left,right) -> string_of_binary " > " left right
  | Expr_Ge (left,right) -> string_of_binary " >= " left right
  | Expr_Func (func,exprs) -> func ^ "(" ^ string_of_list exprs ^ ")"

let string_of_lvalue lvalue =
  match lvalue with
  | LVal_Var str -> str
  | LVal_ARef (str,expr) -> (Printf.sprintf "%s[%s]" str
                             (string_of_expr false expr))

let rec print_expr out wrap expr =
  let needs_wrap = function
    | Expr_Bool _ -> false
    | Expr_Int _ -> false
    | Expr_Var _ -> false
    | Expr_Array_Ref _ -> false
    | _ -> true in
  let print_unary op ex =
    Printf.fprintf out "%s" (if wrap then "(" else "");
    Printf.fprintf out "%s" op;
    print_expr out (needs_wrap ex) ex;
    Printf.fprintf out "%s" (if wrap then ")" else "") in
  let print_binary op left right =
    Printf.fprintf out "%s" (if wrap then "(" else "");
    print_expr out true left;
    Printf.fprintf out "%s" op;
    print_expr out true right;
    Printf.fprintf out "%s" (if wrap then ")" else "") in
  let print_list exprs =
    match exprs with
    | [] -> ()
    | [ex] -> print_expr out false ex
    | ex :: rest ->
      (print_expr out false ex;
       List.iter (fun ex ->
           Printf.fprintf out ",";
           print_expr out false ex) rest) in        
  match expr with
  | Expr_Bool true -> Printf.fprintf out "TRUE"
  | Expr_Bool false -> Printf.fprintf out "FALSE"
  | Expr_Int i -> Printf.fprintf out "%d" i
  | Expr_Var sym -> Printf.fprintf out "%s" sym
  | Expr_Array expr_list -> (Printf.fprintf out "[";
                             print_list expr_list;
                             Printf.fprintf out "]")
  | Expr_Array_Ref (ex1,ex2) -> (print_expr out false ex1;
                                 Printf.fprintf out "[";
                                 print_expr out false ex2;
                                 Printf.fprintf out "]")
  | Expr_Add (left,right) -> print_binary " + " left right
  | Expr_Sub (left,right) -> print_binary " - " left right
  | Expr_Mul (left,right) -> print_binary " * " left right
  | Expr_Div (left,right) -> print_binary " / " left right
  | Expr_Mod (left,right) -> print_binary " mod " left right
  | Expr_Neg ex -> print_unary "-" ex
  | Expr_Not ex -> print_unary "!" ex
  | Expr_And (left,right) -> print_binary " & " left right
  | Expr_Or (left,right) -> print_binary " | " left right
  | Expr_Xor (left,right) -> print_binary " xor " left right
  | Expr_Eq (left,right) -> print_binary " = " left right
  | Expr_Ne (left,right) -> print_binary " != " left right
  | Expr_Lt (left,right) -> print_binary " < " left right
  | Expr_Le (left,right) -> print_binary " <= " left right
  | Expr_Gt (left,right) -> print_binary " > " left right
  | Expr_Ge (left,right) -> print_binary " >= " left right
  | Expr_Func (func,exprs) -> (Printf.fprintf out "%s(" func;
                               print_list exprs;
                               Printf.fprintf out ")")
                               

let print_case_result out result =
  let print_next_choice expr =
    (Printf.fprintf out ", "; print_expr out false expr) in
  match result with
  | Single expr -> print_expr out false expr
  | Choose [] -> Printf.fprintf out "{ }"
  | Choose [ex] -> (Printf.fprintf out "{ ";
                    print_expr out false ex;
                    Printf.fprintf out " }")
  | Choose (first :: rest) ->
    (Printf.fprintf out "{ ";
     print_expr out false first;
     List.iter print_next_choice rest;
     Printf.fprintf out " }")
    
let print_case out indent (Case (test,res)) =
  Printf.fprintf out "%s" indent;
  print_expr out false test;
  Printf.fprintf out " : ";
  print_case_result out res;
  Printf.fprintf out ";\n"

let print_stmt_val out indent stmt_val =
  match stmt_val with
  | Val_Case case_exprs ->
    (Printf.fprintf out "\n%s  case\n" indent;
     List.iter (print_case out (indent ^ "    ")) case_exprs;
     Printf.fprintf out "%s  esac" indent)
  | Val_Expr expr -> print_expr out false expr

let print_lvalue out lvalue =
  match lvalue with
  | LVal_Var str -> Printf.fprintf out "%s" str
  | LVal_ARef (str,expr) -> (Printf.fprintf out "%s[" str;
                             print_expr out false expr;
                             Printf.fprintf out "]")
let print_stmt out indent stmt =
  match stmt with
  | Init (lvalue,stmt_val) -> (Printf.fprintf out "%sinit(" indent;
                               print_lvalue out lvalue;
                               Printf.fprintf out ") := ";
                               print_stmt_val out indent stmt_val;
                               Printf.fprintf out ";\n")
  | Next (lvalue,stmt_val) -> (Printf.fprintf out "%snext(" indent;
                               print_lvalue out lvalue;
                               Printf.fprintf out ") := ";
                               print_stmt_val out indent stmt_val;
                               Printf.fprintf out ";\n")
   
let print_ltl out indent ltl =
  match ltl with
  | Ltl (spec,expr) ->
    (Printf.fprintf out "%sLTLSPEC %s " indent spec;
     print_expr out true expr;
     Printf.fprintf out ";\n")
  | Invarspec expr ->
    (Printf.fprintf out "%sINVARSPEC " indent;
     print_expr out true expr;
     Printf.fprintf out ";\n")

let print_definition out indent (Definition (lvalue,expr)) =
  Printf.fprintf out "%s" indent;
  print_lvalue out lvalue;
  Printf.fprintf out " := ";
  print_expr out false expr;
  Printf.fprintf out ";\n"
                                 
let print_definitions out defines =
  match defines with
  | [] -> ()
  | deflist -> (Printf.fprintf out "  DEFINE\n";
                List.iter (print_definition out "    ") deflist)

let print_program out (Program (name, decls, defines, stmts, ltls)) =
  Printf.fprintf out "MODULE %s\n" name;
  Printf.fprintf out "  VAR\n";
  List.iter (print_decl out "    ") decls;
  print_definitions out defines;
  Printf.fprintf out "  ASSIGN\n";
  List.iter (print_stmt out "    ") stmts;
  List.iter (print_ltl out "  ") ltls
  
let rec walk_expr func context expr =
  match expr with
  | Expr_Bool _ -> func context expr
  | Expr_Int _ -> func context expr
  | Expr_Var _ -> func context expr
  | Expr_Array exprs ->
    let top_ctx = List.fold_left (walk_expr func) context exprs in
    func top_ctx expr
  | Expr_Func (_, exprs) ->
    let top_ctx = List.fold_left (walk_expr func) context exprs in
    func top_ctx expr
  | Expr_Array_Ref (arr, ind) ->
    let arr_ctx = walk_expr func context arr in
    let ind_ctx = walk_expr func arr_ctx ind in
    func ind_ctx expr
  | Expr_Add (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Sub (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Mul (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Div (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Mod (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_And (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Or (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Xor (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Eq (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Ne (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Lt (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Gt (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Le (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Ge (left, right) ->
    let left_ctx = walk_expr func context left in
    let right_ctx = walk_expr func left_ctx right in
    func right_ctx expr
  | Expr_Neg ex1 ->
    let neg_ctx = walk_expr func context ex1 in
    func neg_ctx expr
  | Expr_Not ex1 ->
    let not_ctx = walk_expr func context ex1 in
    func not_ctx expr

let walk_statement_exprs func context stmt =
  let walk_case_expr context case_expr =
    match case_expr with
    | Case (expr, Single result_expr) ->
      walk_expr func (walk_expr func context expr) result_expr
    | Case (expr, Choose result_exprs) ->
      List.fold_left (walk_expr func) (walk_expr func context expr)
        result_exprs
  in
  match stmt with
  | Init (_, Val_Expr expr) -> walk_expr func context expr
  | Init (_, Val_Case case_exprs) ->
    List.fold_left walk_case_expr context case_exprs
  | Next (_, Val_Expr expr) -> walk_expr func context expr
  | Next (_, Val_Case case_exprs) ->
    List.fold_left walk_case_expr context case_exprs

let rec eval_expr expr =
  match expr with
  | Expr_Int i -> Some i
  | Expr_Add (left,right) ->
    (match (eval_expr left, eval_expr right) with
    | (Some i, Some j) -> Some (i+j)
    | _ -> None)
  | Expr_Sub (left,right) ->
    (match (eval_expr left, eval_expr right) with
    | (Some i, Some j) -> Some (i-j)
    | _ -> None)
  | Expr_Mul (left,right) ->
    (match (eval_expr left, eval_expr right) with
    | (Some i, Some j) -> Some (i*j)
    | _ -> None)
  | Expr_Div (left,right) ->
    (match (eval_expr left, eval_expr right) with
    | (Some i, Some j) -> Some (i/j)
    | _ -> None)
  | Expr_Mod (left,right) ->
    (match (eval_expr left, eval_expr right) with
    | (Some i, Some j) -> Some (i mod j)
    | _ -> None)
  | _ -> None
    
let rec make_boolean expr =
  match expr with
  | Expr_Int _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Var _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Array _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Array_Ref _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Func _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Add _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Sub _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Mul _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Div _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Mod _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Neg _ -> Expr_Ne (expr, Expr_Int 0)
  | Expr_Not arg -> invert_expr (make_boolean arg)
  | Expr_And (l,r) -> Expr_And (make_boolean l, make_boolean r)
  | Expr_Or (l,r) -> Expr_Or (make_boolean l, make_boolean r)
  | Expr_Xor (l,r) -> Expr_Xor (make_boolean l, make_boolean r)
  | _ -> expr

and invert_expr expr =
  match expr with
  | Expr_Eq (l,r) -> Expr_Ne (l,r)
  | Expr_Ne (l,r) -> Expr_Eq (l,r)
  | Expr_Gt (l,r) -> Expr_Le (l,r)
  | Expr_Ge (l,r) -> Expr_Lt (l,r)
  | Expr_Lt (l,r) -> Expr_Ge (l,r)
  | Expr_Le (l,r) -> Expr_Gt (l,r)
  | Expr_Not arg -> arg
  | Expr_And (l, r) -> Expr_Or (invert_expr l, invert_expr r)
  | Expr_Or (l, r) -> Expr_And (invert_expr l, invert_expr r)
  | ex -> Expr_Not ex
            
let compare_expr expr1 expr2 =
  String.compare (string_of_expr false expr1) (string_of_expr false expr2)

module ExprKey = struct
  type t = expr_type
  let compare = compare_expr
end

