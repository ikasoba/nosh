module Nosh.lang.runtime.Runtime

open System
open System.IO
open System.Reflection
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Text.RegularExpressions
open Nosh.lang.Ast
open Nosh.lang.runtime.Scope

open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Operators
open FSharp.Interop.Dynamic.SymbolicString

type EvaluateState =
    { redirectStdout: bool
      redirectStdin: bool }

type NativeMethod(parent: obj, methods: MethodInfo seq) =
    member this.Invoke(args: obj array) =
        let method =
            methods
            |> Seq.tryFind (fun method ->
                let paramInfos = method.GetParameters()

                args.Length = paramInfos.Length
                && paramInfos |> Seq.forall2 (fun (x: obj) y -> x.GetType() = y.ParameterType) args)

        match method with
        | Some(method) -> method.Invoke(parent, args)
        | _ -> null

type RuntimeError(message: string) =
    inherit Exception(message)

type NoshFunctionInfo =
    { scope: Scope
      runtime: Runtime
      input: obj }

and NoshFunctionAsync = (NoshFunctionInfo * (obj array)) -> Async<obj>
and NoshFunction = (NoshFunctionInfo * (obj array)) -> obj

and Runtime() as this =
    let globalScope = Scope(Map.empty, None)

    do
        let shellPromptFn: NoshFunction =
            fun (info, _) ->
                let previousExitStatus =
                    let previousExitCode =
                        if info.scope.has "ExitCode" then
                            info.scope.get "ExitCode" |> unbox
                        else
                            0

                    if previousExitCode > 0 then
                        // タイプミスの様に見えるけど、末尾のスペースは本当に必要なやつだよ
                        "\x1b[91m⬤\x1b[0m "
                    else
                        "\x1b[90m〇\x1b[0m"

                let cwd =
                    let currentPath = Directory.GetCurrentDirectory()
                    let homePath = Environment.GetEnvironmentVariable "HOME"

                    if currentPath = homePath then "~" else currentPath

                let user = Environment.UserName

                $"{previousExitStatus} \x1b[92m{user}\x1b[0m \x1b[93m{cwd}\x1b[0m\n➤➤ " |> unbox

        let printFn: NoshFunction =
            fun (info, args) ->
                this.reprWrite (Array.concat [ [| info.input |]; args ])

                null :> obj

        let raiseFn: NoshFunction =
            fun (info, args) ->
                let err: exn =
                    if args.Length > 0 && args[0] :? exn then args[0] |> unbox
                    elif args.Length > 0 then Exception $"{args[0]}"
                    else Exception $"{info.input}"

                raise err :> obj

        let clearFn: NoshFunction =
            fun (info, args) ->
                printf "\x1b[2J\x1b[1;1H"
                ()

        let inputFn: NoshFunction =
            fun (info, args) ->
                if args.Length > 0 then
                    printf "%s" (args[0].ToString())
                else
                    ()

                stdin.ReadLine()

        this.GlobalScope.set "ShellPrompt" shellPromptFn

        this.GlobalScope.set "print" printFn

        this.GlobalScope.set "input" inputFn

        this.GlobalScope.set "raise" raiseFn

        this.GlobalScope.set "clear" clearFn

    member this.GlobalScope: Scope = globalScope

    member this.createEmptyScope() =
        let m = new Map<string, obj>([])
        new Scope(m, option.None)

    member this.evalAstList (body: Ast list) (scope: Scope) (state: EvaluateState) =
        async {
            let mutable result = null
            let mutable breakFlag = false
            let mutable i = 0

            while (not breakFlag) && i < body.Length do
                let expr = body[i]

                match expr with
                | ReturnStmt(_) -> breakFlag <- true
                | _ -> ()

                let! value = this.evalAst expr scope state
                result <- value

                if result :? Process then
                    let tmp: Process = unbox result

                    if not tmp.HasExited then
                        let! tmp2 = this.processOutputReadToEnd tmp
                        result <- tmp2
                    else
                        result <- ""
                else
                    ()

                i <- i + 1

            return result
        }

    member this.reprWrite(value: obj) =
        let openStdout = Console.OpenStandardOutput()

        if value :? byte array then
            let value: byte array = value |> unbox
            openStdout.Write(value, 0, value.Length) |> ignore
        elif value :? string then
            let value: string = value |> unbox
            printfn "%s" value
        elif value :? Collections.Generic.IEnumerable<obj> then
            let value = value |> unbox<Collections.Generic.IEnumerable<obj>> |> List.ofSeq

            let mutable i = 0

            for item in value do
                this.reprWrite item
        elif value = null || value = () then
            ()
        else
            printfn "%A" value

        openStdout.Dispose()

    member this.processOutputReadToEnd(proc: Process) =
        async {
            // TODO: StringBuilderを使う
            let mutable output = ""

            if (not proc.StartInfo.RedirectStandardOutput) then
                let! _ = proc.WaitForExitAsync() |> Async.AwaitTask

                ()
            else
                let! _ = proc.WaitForExitAsync() |> Async.AwaitTask
                let! res = proc.StandardOutput.ReadToEndAsync() |> Async.AwaitTask
                output <- res

            proc.Dispose()

            return Regex.Replace(output, "[\r\n]\x00?$", "")
        }

    member this.createProcess (fileName: string) (args: obj array) (scope: Scope) (state: EvaluateState) =
        async {
            let proc = new Process()

            proc.StartInfo.FileName <- fileName

            proc.StartInfo.RedirectStandardOutput <- state.redirectStdout
            proc.StartInfo.RedirectStandardInput <- state.redirectStdin

            for item in args do
                proc.StartInfo.ArgumentList.Add(item.ToString())

            proc.Start() |> ignore

            return proc
        }

    member this.runInvokeExpr (expr: Ast) (scope: Scope) (input: obj) (state: EvaluateState) =
        async {
            match expr with
            | InvokeExpr(expr, args) ->
                let! value =
                    match expr with
                    | IdentLiteral(ident) ->
                        if (scope.has ident) then
                            async { return scope.get ident }
                        else
                            async { return ident }
                    | _ -> this.evalAst expr scope state

                let! argList = List.map (fun x -> this.evalAst x scope state) args |> Async.Parallel

                if value :? NativeMethod then
                    let method: NativeMethod = value |> unbox

                    return method.Invoke(argList)
                elif value :? NoshFunctionAsync then
                    let func: NoshFunctionAsync = value |> unbox

                    let! result =
                        func (
                            { input = input
                              scope = scope
                              runtime = this },
                            argList
                        )

                    return result
                elif value :? NoshFunction then
                    let func: NoshFunction = value |> unbox

                    let result =
                        func (
                            { input = input
                              scope = scope
                              runtime = this },
                            argList
                        )

                    return result
                else

                    let fileName = $"{value}"

                    let! proc = this.createProcess fileName argList scope state

                    return proc
        }

    member this.readToEndIfProcess(value: obj) : Async<obj> =
        async {
            if value :? Process then
                let tmp: Process = unbox value

                let! tmp2 = this.processOutputReadToEnd tmp
                return tmp2
            elif value :? Async<obj> then
                let! proc = unbox<obj Async> value
                let! result = this.readToEndIfProcess proc

                return result
            else
                return value
        }

    member this.evalAst (expr: Ast) (scope: Scope) (state: EvaluateState) : Async<obj> =
        async {
            match expr with
            | IdentLiteral(value) -> return scope.get value
            | StringLiteral(value) -> return value
            | NumberLiteral(value) -> return value

            | PathLiteral(value) ->
                if IO.File.Exists value then
                    return IO.FileInfo value
                elif IO.Directory.Exists value then
                    return IO.DirectoryInfo value
                else
                    return RuntimeError "ファイル、フォルダーが存在しない" |> raise

            | InvokeExpr(_, _) ->
                let! result = this.runInvokeExpr expr scope () state
                let mutable result = result

                return result

            | AddOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?+? y
            | SubOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?-? y
            | MulOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?*? y
            | DivOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?/? y

            | ModOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?%? y

            | AssignOperator(x, y) ->
                match x with
                | IdentLiteral(name) ->
                    let! value =
                        this.evalAst
                            y
                            scope
                            { redirectStdin = state.redirectStdin
                              redirectStdout = true }
                        |> this.readToEndIfProcess

                    scope.set name value |> ignore

                    return value
                | _ -> return raise (RuntimeError $"cannot assign to {x}")

            | EqualOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x = y

            | NotEqualOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x <> y

            | LtOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?<? y

            | LeOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?<=? y

            | GtOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?>? y

            | GeOperator(x, y) ->
                let! x = this.evalAst x scope state |> this.readToEndIfProcess
                let! y = this.evalAst y scope state |> this.readToEndIfProcess

                return x ?>=? y

            | PropertyAccessOperator(x, y) ->
                let! value = this.evalAst x scope state |> this.readToEndIfProcess

                let key =
                    match y with
                    | IdentLiteral(ident) -> ident
                    | _ -> ""

                let property = value.GetType().GetProperty(key)

                if property <> null then
                    return property.GetValue(x, null)
                else

                    let methods =
                        value.GetType().GetMethods() |> Seq.filter (fun x -> x.Name = key) |> Seq.toList

                    if methods.Length = 0 then
                        return null
                    else
                        return NativeMethod(value, methods)

            | PipelineOperator(x, y) ->
                let! x =
                    async {
                        match x with
                        | InvokeExpr(_, _) ->
                            let! result = this.runInvokeExpr x scope () state
                            return result
                        | _ ->
                            let! result = this.evalAst x scope state
                            return result
                    }

                let! y =
                    this.evalAst
                        y
                        scope
                        { redirectStdin = true
                          redirectStdout = state.redirectStdout }

                if y :? NoshFunctionAsync then
                    let y: NoshFunctionAsync = y |> unbox

                    let! result =
                        y (
                            { input = ()
                              scope = scope
                              runtime = this },
                            [||]
                        )

                    return result

                elif y :? NoshFunction then
                    let y: NoshFunction = y |> unbox

                    let result =
                        y (
                            { input = ()
                              scope = scope
                              runtime = this },
                            [||]
                        )

                    return result

                elif x :? IO.StreamReader && y :? Process then
                    let x: IO.StreamReader = x |> unbox
                    let y: Process = y |> unbox

                    let buf: byte array = Array.zeroCreate 256

                    while not x.EndOfStream do
                        let bufMemory = Memory buf
                        let! length = x.BaseStream.ReadAsync bufMemory |> fun x -> x.AsTask() |> Async.AwaitTask

                        let! _ =
                            y.StandardInput.BaseStream.WriteAsync buf[..length]
                            |> fun x -> x.AsTask() |> Async.AwaitTask

                        ()

                    return y

                elif x :? IO.StreamReader && y :? IO.StreamWriter then
                    let x: IO.StreamReader = x |> unbox
                    let y: IO.StreamWriter = y |> unbox

                    let buf: byte array = Array.zeroCreate 256

                    while not x.EndOfStream do
                        let bufMemory = Memory buf

                        let! length = x.BaseStream.ReadAsync bufMemory |> fun x -> x.AsTask() |> Async.AwaitTask
                        let! _ = y.BaseStream.WriteAsync buf[..length] |> fun x -> x.AsTask() |> Async.AwaitTask
                        ()

                    return y

                elif y :? Process then
                    let y: Process = y |> unbox

                    y.StandardInput.Write x
                    y.StandardInput.Close()

                    return y

                elif y :? IO.StreamWriter then
                    let y: IO.StreamWriter = y |> unbox

                    y.Write x

                    return y

                else
                    return (raise (RuntimeError "不明なエラー"))

            | DefineFunctionStmt(name, argNames, body) ->
                let name = name |> Ast.getIdentName

                let argNames = argNames |> List.map (fun x -> x |> Ast.getIdentName)

                let func: NoshFunctionAsync =
                    fun (info, args) ->
                        async {
                            let funcScope = Scope(Map.empty, Some(scope))

                            funcScope.set "input" info.input

                            if argNames.Length <> args.Length then
                                return raise (RuntimeError "invalid arguments length")
                            else
                                let mutable i = 0

                                while i < args.Length do
                                    let name = argNames[i]
                                    let value = args[i]

                                    funcScope.set name value

                                    i <- i + 1

                            let! result = this.evalAstList body funcScope state
                            return result
                        }

                scope.set name func
                return func

            | IfStmt(cond, body, elseBody) ->
                let! value = this.evalAst cond scope state
                let value = Convert.ToBoolean value

                if value then
                    let! result = this.evalAstList body scope state
                    return result
                else

                    match elseBody with
                    | Some(elseBody) ->
                        let! result = this.evalAstList elseBody scope state
                        return result
                    | None -> return null
        }
