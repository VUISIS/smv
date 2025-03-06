
type module_type_type = ModType of string * (basic_expr_type list)
                      | ModProcessType of string * (basic_expr_type list)
and simple_type_type = TypeBoolean | TypeWord of int | TypeUnsignedWord of int |
                       TypeEnum of enum_val_type | TypeRange of int * int |
                       TypeArray of int * int * simple_type_type
and type_type = TypeSimple of simple_type_type | TypeModule of module_type_type
and enum_val_type = EnumInt of int | EnumSym of string
and constant_type = ConstBool of bool | ConstInt of int | ConstSym of complex_id_type |
                    ConstWord of int | ConstRange of int * int
and complex_id_type = IdSym of string | IdDot of complex_id_type * string
                      | IdRef of complex_id_type * basic_expr_type
                      | IdSelf
and case_type = Case of basic_expr_type * basic_expr_type
and basic_expr_type = ExprConst of constant_type
                    | ExprVar of complex_id_type
                    | ExprDef of complex_id_type
                    | ExprAbs of basic_expr_type
                    | ExprMax of basic_expr_type * basic_expr_type
                    | ExprMin of basic_expr_type * basic_expr_type
                    | ExprNot of basic_expr_type
                    | ExprAnd of basic_expr_type * basic_expr_type
                    | ExprOr of basic_expr_type * basic_expr_type
                    | ExprXor of basic_expr_type * basic_expr_type
                    | ExprXnor of basic_expr_type * basic_expr_type
                    | ExprImplies of basic_expr_type * basic_expr_type
                    | ExprEquiv of basic_expr_type * basic_expr_type
                    | ExprEqual of basic_expr_type * basic_expr_type
                    | ExprNotEqual of basic_expr_type * basic_expr_type
                    | ExprLess of basic_expr_type * basic_expr_type
                    | ExprLessEqual of basic_expr_type * basic_expr_type
                    | ExprGreater of basic_expr_type * basic_expr_type
                    | ExprGreaterEqual of basic_expr_type * basic_expr_type
                    | ExprNeg of basic_expr_type
                    | ExprPlus of basic_expr_type * basic_expr_type
                    | ExprMinus of basic_expr_type * basic_expr_type
                    | ExprTimes of basic_expr_type * basic_expr_type
                    | ExprDiv of basic_expr_type * basic_expr_type
                    | ExprMod of basic_expr_type * basic_expr_type
                    | ExprShiftLeft of basic_expr_type * basic_expr_type
                    | ExprShiftRight of basic_expr_type * basic_expr_type
                    | ExprArrayRef of basic_expr_type * basic_expr_type
                    | ExprBitSel of basic_expr_type * int * int
                    | ExprConcat of basic_expr_type * basic_expr_type
                    | ExprWord1 of basic_expr_type
                    | ExprBool of basic_expr_type
                    | ExprToInt of basic_expr_type
                    | ExprSigned of basic_expr_type
                    | ExprUnsigned of basic_expr_type
                    | ExprExtend of basic_expr_type * basic_expr_type
                    | ExprResize of basic_expr_type * basic_expr_type
                    | ExprUnion of basic_expr_type * basic_expr_type
                    | ExprCount of basic_expr_type list
                    | ExprSet of basic_expr_type list
                    | ExprIn of basic_expr_type * basic_expr_type
                    | ExprTernary of basic_expr_type * basic_expr_type * basic_expr_type
                    | ExprCase of case_type list
                    | ExprNext of basic_expr_type

and var_decl_type = VarDecl of complex_id_type * type_type
and ivar_decl_type = IVarDecl of complex_id_type * simple_type_type
and frozenvar_decl_type = FrozenVarDecl of complex_id_type * simple_type_type
and def_decl_type = DefineDecl of complex_id_type * basic_expr_type
and assign_type = AssignVar of complex_id_type * basic_expr_type
                     | AssignInit of complex_id_type * basic_expr_type
                     | AssignNext of complex_id_type * basic_expr_type
and fairness_type = FairnessFairness of basic_expr_type
                  | FairnessJustice of basic_expr_type
                  | FairnessCompassion of basic_expr_type
and ctl_expr_type = CtlExprExpr of basic_expr_type
                  | CtlExprNot of ctl_expr_type
                  | CtlExprAnd of ctl_expr_type * ctl_expr_type
                  | CtlExprOr of ctl_expr_type * ctl_expr_type
                  | CtlExprXor of ctl_expr_type * ctl_expr_type
                  | CtlExprXnor of ctl_expr_type * ctl_expr_type
                  | CtlExprImplies of ctl_expr_type * ctl_expr_type
                  | CtlExprEquiv of ctl_expr_type * ctl_expr_type
                  | CtlExprExistsGlobal of ctl_expr_type
                  | CtlExprExistsNext of ctl_expr_type
                  | CtlExprExistsFinal of ctl_expr_type
                  | CtlExprForallGlobal of ctl_expr_type
                  | CtlExprForallNext of ctl_expr_type
                  | CtlExprForallFinal of ctl_expr_type
                  | CtlExprExistsUntil of ctl_expr_type * ctl_expr_type
                  | CtlExprForallUntil of ctl_expr_type * ctl_expr_type
and ctl_spec_type = CtlCtlSpec of ctl_expr_type
                  | CtlSpec of ctl_expr_type
                  | CtlCtlSpecName of string * ctl_expr_type
                  | CtlSpecName of string * ctl_expr_type
and rtctl_expr_type = RtctlCtlExpr of ctl_expr_type
                    | RtctlEBF of int * int * rtctl_expr_type
                    | RtctlABF of int * int * rtctl_expr_type
                    | RtctlEBG of int * int * rtctl_expr_type
                    | RtctlA of rtctl_expr_type * int * int * rtctl_expr_type
                    | RtctlE of rtctl_expr_type * int * int * rtctl_expr_type
and rtctl_spec_type = RtctlCtlSpec of rtctl_expr_type
                    | RtctlSpec of rtctl_expr_type
                    | RtctlCtlSpecName of string * rtctl_expr_type
                    | RtctlSpecName of string * rtctl_expr_type
and invar_spec_type = InvarSpec of basic_expr_type
                    | InvarSpecName of string * basic_expr_type
and ltl_expr_type = LtlExprExpr of basic_expr_type
                  | LtlExprNot of ltl_expr_type
                  | LtlExprAnd of ltl_expr_type * ltl_expr_type
                  | LtlExprOr of ltl_expr_type * ltl_expr_type
                  | LtlExprXor of ltl_expr_type * ltl_expr_type
                  | LtlExprXnor of ltl_expr_type * ltl_expr_type
                  | LtlExprImplies of ltl_expr_type * ltl_expr_type
                  | LtlExprEquiv of ltl_expr_type * ltl_expr_type
                  | LtlNext of ltl_expr_type
                  | LtlGlobal of ltl_expr_type
                  | LtlFinally of ltl_expr_type
                  | LtlUntil of ltl_expr_type * ltl_expr_type
                  | LtlReleases of ltl_expr_type * ltl_expr_type
                  | LtlPrevious of ltl_expr_type
                  | LtlNotPrevious of ltl_expr_type
                  | LtlHistorically of ltl_expr_type
                  | LtlOnce of ltl_expr_type
                  | LtlSince of ltl_expr_type * ltl_expr_type
                  | LtlTriggered of ltl_expr_type * ltl_expr_type
and compute_expr_type = ComputeMin of rtctl_expr_type * rtctl_expr_type
                      | ComputeMax of rtctl_expr_type * rtctl_expr_type
and ltl_spec_type = LtlSpec of ltl_expr_type
                  | LtlSpecName of string * ltl_expr_type
and compute_spec_type = ComputeSpec of compute_expr_type
                      | CompteSpecName of string * compute_expr_type
type mod_elem_type = ModVarDecl of var_decl_type list
                   | ModIVarDecl of ivar_decl_type list
                   | ModFrozenVarDecl of frozenvar_decl_type list
                   | ModDefineDecl of def_decl_type list
                   | ModConstDecl of complex_id_type list
                   | ModAssignConstraint of assign_type
                   | ModTransConstraint of basic_expr_type
                   | ModInitConstraint of basic_expr_type
                   | ModInvarConstraint of basic_expr_type
                   | ModFairnessConstraint of fairness_type
                   | ModCtlSpec of ctl_spec_type
                   | ModRtctlSpec of rtctl_spec_type
                   | ModInvarSpec of invar_spec_type
                   | ModLtlSpec of ltl_spec_type
                   | ModComputeSpec of compute_spec_type
                   | ModIsaDecl of string

type module_type = Module of (string list) * (mod_elem_type list)
