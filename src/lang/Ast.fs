module Nosh.lang.Ast

type Ast =
    | CommentLiteral of string
    | IdentLiteral of string
    | StringLiteral of string
    | PathLiteral of string
    | NumberLiteral of float
    | HexLiteral of string
    // -ABCDE --Abcdef
    | FlagLiteral of string
    // -Flag=1 --flag=1
    | OptionLiteral of string * Ast
    | InvokeExpr of Ast * list<Ast>
    | AddOperator of Ast * Ast
    | SubOperator of Ast * Ast
    | DivOperator of Ast * Ast
    | MulOperator of Ast * Ast
    | ModOperator of Ast * Ast
    | EqualOperator of Ast * Ast
    | NotEqualOperator of Ast * Ast
    | LtOperator of Ast * Ast
    | LeOperator of Ast * Ast
    | GtOperator of Ast * Ast
    | GeOperator of Ast * Ast
    | AssignOperator of Ast * Ast
    | PropertyAccessOperator of Ast * Ast
    | PipelineOperator of Ast * Ast
    | DefineFunctionStmt of Ast * Ast list * Ast list
    | DefineVariableStmt of Ast * Ast
    | ReturnStmt of Ast
    | IfStmt of Ast * Ast list * Ast list option
    | ExportStmt of string * Ast

    static member getIdentName(ast: Ast) =
        match ast with
        | IdentLiteral(ident) -> ident
