module Nosh.lang.OperatorParser

open FParsec

let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let ( *!* ) a label : 'a =
    printfn "parsed %s" label
    a

type Associativity =
    | Left
    | Right

type InfixTerm<'T> =
    { termLeft: Parser<'T Expr, unit>
      termRight: (Parser<'T Expr, unit>) -> Parser<'T Expr, unit> }

and InfixOperator<'T> =
    { ident: string
      priority: int
      assoc: Associativity
      mapFn: ('T -> 'T -> 'T)
      term: 'T InfixTerm option }

and Expr<'T> =
    | Term of 'T
    | Infix of InfixOperator<'T> * Expr<'T> * Expr<'T>

let isErrorReply (reply: Reply<'R>) =
    match reply.Status with
    | Ok -> false
    | _ -> true

type OperatorParser<'T>(whitespace: Parser<unit, unit>, term: Parser<'T, unit>) =
    let mutable infixes: 'T InfixOperator list = []

    let createInfix (op: InfixOperator<'T>) ((left, right): Expr<'T> * Expr<'T>) =
        match right with
        | Infix(rightOp, rightOpLeft, rightOpRight) ->
            if op.priority > rightOp.priority then
                let x = Expr.Infix(op, left, rightOpLeft)
                Expr.Infix(rightOp, x, rightOpRight)
            elif op.ident = rightOp.ident && op.assoc = Left then
                let x = Expr.Infix(op, left, rightOpLeft)
                Expr.Infix(rightOp, x, rightOpRight)
            else
                Expr.Infix(op, left, right)
        | Term(_) -> Expr.Infix(op, left, right)

    let rec extractFromExpr (expr: Expr<'T>) =
        match expr with
        | Term(value) -> value
        | Infix(op, left, right) -> op.mapFn (extractFromExpr left) (extractFromExpr right)

    member this.addInfix (ident: string) (priority: int) (assoc: Associativity) (map: ('T -> 'T -> 'T)) =
        infixes <-
            List.append
                infixes
                [ { ident = ident
                    priority = priority
                    assoc = assoc
                    mapFn = map
                    term = None } ]

    member this.addCustomInfix termLeft termRight (ident: string) (priority: int) (assoc: Associativity) (map: ('T -> 'T -> 'T)) =
        infixes <-
            List.append
                infixes
                [ { ident = ident
                    priority = priority
                    assoc = assoc
                    mapFn = map
                    term = Some({
                        termLeft = termLeft
                        termRight = termRight
                    }) } ]

    member this.expr =
        let expr, exprRef = createParserForwardedToRef ()

        let infixParser =
            (List.map
                (fun (op: 'T InfixOperator) ->
                    match op.term with
                        | Some(terms) ->
                            let self, selfRef = createParserForwardedToRef ()
                            selfRef :=
                                terms.termLeft .>> whitespace .>> skipString op.ident .>> whitespace .>>. terms.termRight self
                                |>> (fun (left, right) -> createInfix op (left, right))
                                |> attempt

                            self
                        | None ->
                            term .>> whitespace .>> skipString op.ident .>> whitespace .>>. expr
                            |>> (fun (left, right) -> createInfix op (Expr.Term(left), right))
                            |> attempt)
                infixes)
            |> choice

        exprRef := attempt infixParser <|> (term |>> fun expr -> Expr.Term(expr))
        expr

    member this.parser =
        this.expr |>> fun expr -> extractFromExpr (expr)
