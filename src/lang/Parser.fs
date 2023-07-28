module Nosh.lang.Parser

open Nosh.lang.Ast
open Nosh.lang.OperatorParser
open Nosh.lang.ParserUtil
open FParsec

let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
    fun stream ->
        //printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        //printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let ( *!* ) (a: Ast) label : Ast =
    //printfn "parsed %s" label
    a

let whitespace = skipMany (anyOf " \t\r\n")
let whitespaceWithoutLinebreak = skipMany (anyOf " \t")
let whitespace1 = skipMany1 (anyOf " \t\r\n")

let parseBy p str =
    // run関数はFParsecが用意している、パーサーを実行するための関数
    match run p str with
    | Success(res, _, pos) ->
        if pos.Index <> str.Length then
            failwithf "入力を消費しきれてない i: %A len: %A content: %A" pos.Index str.Length (str.Substring((int) pos.Index))
        else

            res
    | Failure(msg, _, _) -> failwithf "parse error: %s" msg

let specialCharacter =
    set [ '\\'; '"'; '('; ')'; '+'; '-'; '*'; '/'; '&'; '|'; '$'; '.'; '{'; '}' ]

let ignoreCharacter = set [ '"'; '('; ')'; '{'; '}' ]

let whitespaceCharacter = set [ '\r'; '\n'; ' '; '\t' ]

let digitCharacter = set [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

let escapeSequence =
    pstring "\\r" |>> (fun _ -> "\n") <|> pstring "\\n" |>> (fun _ -> "\n")

let commentLiteral =
    pstring "#" >>. manySatisfy (fun c -> not <| whitespaceCharacter.Contains(c))
    |>> (fun x -> CommentLiteral(x))

let stringLiteral: Parser<Ast, unit> =
    let normalChar = regex "[^\\\"]"

    let literal = (many (normalChar <|> escapeSequence))

    between (pstring "\"") (pstring "\"") literal
    |>> (fun x -> StringLiteral(System.String.Join("", x)))

let stringLiteralWithoutQuote: Parser<Ast, unit> =
    let firstChar =
        satisfy (fun c -> not ((whitespaceCharacter + ignoreCharacter + digitCharacter).Contains(c)))
        |>> fun c -> c.ToString()

    let normalChar =
        satisfy (fun c -> not ((whitespaceCharacter + ignoreCharacter).Contains(c)))
        |>> fun c -> c.ToString()

    firstChar .>>. (many (normalChar <|> escapeSequence))
    |>> (fun (x, y) -> StringLiteral(x + System.String.Join("", y)))

let numberLiteral: Parser<Ast, unit> = pfloat |>> fun x -> Ast.NumberLiteral(x)

let varnameLiteral: Parser<Ast, unit> =
    regex "\\$[\\p{Lu}\\p{Ll}\\p{Lt}\\p{Lm}\\p{Lo}_][\\p{Lu}\\p{Ll}\\p{Lt}\\p{Lm}\\p{Lo}\\p{Nd}_]*"
    |>> (fun x -> IdentLiteral(x.Substring 1))

let identLiteral: Parser<Ast, unit> =
    let firstChar =
        satisfy (fun c ->
            not (
                (specialCharacter + whitespaceCharacter + digitCharacter + ignoreCharacter)
                    .Contains(c)
            ))
        |>> fun c -> c.ToString()

    let normalChar =
        satisfy (fun c -> not ((specialCharacter + whitespaceCharacter + ignoreCharacter).Contains(c)))
        |>> fun c -> c.ToString()

    firstChar .>>. (many (normalChar <|> escapeSequence))
    |>> (fun (x, y) -> IdentLiteral(x + System.String.Join("", y)))

let filename: Parser<string, unit> =
    let firstChar =
        satisfy (fun c ->
            not (
                (specialCharacter + whitespaceCharacter + digitCharacter + ignoreCharacter)
                    .Contains(c)
            ))
        |>> fun c -> c.ToString()

    let normalChar =
        satisfy (fun c -> not ((specialCharacter + whitespaceCharacter + ignoreCharacter).Contains(c)))
        |>> fun c -> c.ToString()

    let literal =
        firstChar .>>. (many (normalChar <|> escapeSequence))
        |>> (fun (x, y) -> (x + System.String.Join("", y)))

    (sepBy1 literal (skipChar '.')) |>> (fun x -> System.String.Join(".", x))

let pathLiteral =
    ((pstring "." .>> skipAnyOf "\\/")
     <|> (pstring ".." .>> skipAnyOf "\\/")
     <|> (anyOf "\\/" |>> fun x -> $"{x}")
     <|> (regex "[a-zA-Z]:[\\/]"))
    .>>. sepBy1 (pstring "." <|> pstring ".." <|> filename) (skipAnyOf "\\/")
    |>> fun (x, y) -> PathLiteral $"{x}/{System.String.Join('/', y)}"

let constantExpr =
    pathLiteral <|> numberLiteral <|> varnameLiteral <|> stringLiteral

let withBracket parser =
    (between (pstring "(") (pstring ")") (whitespace >>. parser .>> whitespace))

let expr, exprRef = createParserForwardedToRef<Ast, unit> ()
let stmt, stmtRef = createParserForwardedToRef<Ast, unit> ()

let exprLeftRecursionSafe, exprLeftRecursionSafeRef =
    createParserForwardedToRef<Ast, unit> ()

let invokeExpr, invokeExprRef = createParserForwardedToRef ()
let operatorParser, operatorParserRef = createParserForwardedToRef ()
let defineFunctionStmt, defineFunctionStmtRef = createParserForwardedToRef ()

let astParser =
    whitespace
    >>. (separateBy ((commentLiteral) <|> stmt <|> expr) (whitespaceWithoutLinebreak .>> anyOf "\r\n;" >>. whitespace))
    .>> whitespace

let block = skipChar '{' >>. astParser .>> skipChar '}'

let propertyOperator =
    let chain, chainRef = createParserForwardedToRef ()

    chainRef
    := (identLiteral <!> "id") .>> skipString "."
       .>>. ((attempt chain) <|> (identLiteral <!> "id"))
       |>> (fun (x, y) ->
           match y with
           | PropertyAccessOperator(yx, yy) -> PropertyAccessOperator(PropertyAccessOperator(x, yx), yy)
           | _ -> PropertyAccessOperator(x, y))
       <!> "chain"

    ((attempt (withBracket expr) <!> "withbracket") <|> (constantExpr <!> "constant"))
    .>> skipString "."
    .>>. ((attempt chain) <|> (identLiteral <!> "id"))
    |>> (fun (x, y) ->
        match y with
        | PropertyAccessOperator(yx, yy) -> PropertyAccessOperator(PropertyAccessOperator(x, yx), yy)
        | _ -> PropertyAccessOperator(x, y))
    <!> "prop"

do
    let command =
        (skipString "&" .>> whitespace >>. (withBracket expr) |> attempt)
        <|> (attempt pathLiteral)
        <|> identLiteral

    invokeExprRef
    := (attempt (
        pipe2
            command
            (whitespace1
             >>. (separateBy
                 ((attempt (withBracket expr))
                  <|> (attempt constantExpr)
                  <|> stringLiteralWithoutQuote)
                 (whitespace1)))
            (fun x y -> InvokeExpr(x, y))
       ))
       <|> (command |>> fun x -> InvokeExpr(x, []))
       <!> "invk"

do
    let term = ((attempt exprLeftRecursionSafe) <|> (withBracket expr))

    let opp = new OperatorParser<Ast>(whitespace, term)
    let oppExpr, oppExprRef = createParserForwardedToRef ()

    opp.addInfix "/" 4 (OperatorParser.Associativity.Left) (fun x y -> DivOperator(x, y))
    opp.addInfix "%" 4 (OperatorParser.Associativity.Left) (fun x y -> ModOperator(x, y))
    opp.addInfix "*" 4 (OperatorParser.Associativity.Left) (fun x y -> MulOperator(x, y))

    opp.addInfix "+" 3 (OperatorParser.Associativity.Left) (fun x y -> AddOperator(x, y))
    opp.addInfix "-" 3 (OperatorParser.Associativity.Left) (fun x y -> SubOperator(x, y))

    opp.addInfix ">" 2 (OperatorParser.Associativity.Left) (fun x y -> GtOperator(x, y))
    opp.addInfix ">=" 2 (OperatorParser.Associativity.Left) (fun x y -> GeOperator(x, y))
    opp.addInfix "<" 2 (OperatorParser.Associativity.Left) (fun x y -> LtOperator(x, y))
    opp.addInfix "<=" 2 (OperatorParser.Associativity.Left) (fun x y -> LeOperator(x, y))

    opp.addInfix "==" 1 (OperatorParser.Associativity.Left) (fun x y -> EqualOperator(x, y))
    opp.addInfix "!=" 1 (OperatorParser.Associativity.Left) (fun x y -> NotEqualOperator(x, y))

    opp.addCustomInfix
        ((varnameLiteral) |>> fun x -> Term x)
        (fun _ -> oppExpr)
        "="
        0
        (OperatorParser.Associativity.Right)
        (fun x y -> AssignOperator(x, y))

    operatorParserRef := opp.parser
    oppExprRef := opp.expr

do
    let argNames = separateBy (varnameLiteral <|> identLiteral) whitespace1

    let functionBody =
        block <|> (skipString "->" .>> whitespace >>. expr |>> fun x -> [ x ])

    defineFunctionStmtRef
    := skipString "fun" .>> whitespace1 >>. (varnameLiteral <|> identLiteral)
       .>> whitespace
       .>>. argNames
       .>> whitespace
       .>>. functionBody
       |>> (fun ((name, args), body) -> DefineFunctionStmt(name, args, body))
       <!> "deffunc"

let returnStmt =
    skipString "return" >>. whitespace >>. expr |>> fun x -> ReturnStmt x

let ifStmt, ifStmtRef = createParserForwardedToRef ()

do
    let body = block <|> (skipString "->" .>> whitespace >>. expr |>> fun x -> [ x ])

    ifStmtRef
    := skipString "if" >>. whitespace >>. expr .>> whitespace1 .>>. body .>> whitespace
       .>>. opt (
           skipString "else"
           >>. whitespace
           >>. ((attempt ifStmt |>> fun x -> [ x ]) <|> block)
       )
       |>> fun ((cond, body), elseBody) -> IfStmt(cond, body, elseBody)

let pipelineOperator, pipelineOperatorRef = createParserForwardedToRef ()

do
    let term = exprLeftRecursionSafe

    pipelineOperatorRef
    := term .>> whitespace .>> skipString "|>" .>> whitespace
       .>>. ((attempt pipelineOperator) <|> term)
       |>> (fun (x, y) ->
           match y with
           | PipelineOperator(yx, yy) -> PipelineOperator(PipelineOperator(x, yx), yy)
           | _ -> PipelineOperator(x, y))

exprLeftRecursionSafeRef
:= (attempt invokeExpr)
   <|> (attempt identLiteral)
   <|> (attempt (withBracket expr))
   <|> (constantExpr)

exprRef
:= (attempt propertyOperator)
   <|> (attempt pipelineOperator)
   <|> (attempt operatorParser)
   <|> (attempt exprLeftRecursionSafe)

stmtRef := defineFunctionStmt <|> returnStmt <|> ifStmt
