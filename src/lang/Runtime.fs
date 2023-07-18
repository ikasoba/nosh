module Nosh.lang.runtime.Runtime

open System
open System.Reflection
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open Nosh.lang.Ast

open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Operators
open FSharp.Interop.Dynamic.SymbolicString

type NoshFunctionAsync = (obj array) -> Async<obj>
type NoshFunction = (obj array) -> obj

type NativeMethod (parent: obj, methods: MethodInfo seq) =
    member this.Invoke (args: obj array) =
        let method =
            methods |> Seq.tryFind (fun method ->
                let paramInfos = method.GetParameters()
                args.Length = paramInfos.Length && paramInfos |> Seq.forall2 (fun (x: obj) y -> x.GetType() = y.ParameterType) args)

        match method with
        | Some(method) ->
                method.Invoke(parent, args)
        | _ -> null

type RuntimeError(message: string) =
    inherit Exception()

    member this.message = message
    member this.ToString = message

type Scope(_variables: Map<string, obj>, parent: Scope option) =

    let mutable _variables = _variables

    static member toMap (scope: Scope) : Map<string, obj> =
        let mutable map: Map<string, obj> = scope.variables

        match scope.parent with
        | Some(parent) ->
            let mutable parentMap = Scope.toMap parent

            for k in map.Keys do
                parentMap <- parentMap.Add (k, map[k])

            map <- parentMap

            ()
        | _ ->
            ()

        map

    member this.parent = parent
    member this.variables
        with get () = _variables
        and  set (value) = _variables <- value

    member this.get(key: string) =
        if parent.IsSome && not <| this.hasOwn key then
            parent.Value.get key
        else
            this.variables[key]

    member this.hasOwn(key: string) = this.variables.ContainsKey(key)

    member this.has(key: string) =
        if parent.IsSome && not <| this.hasOwn (key) then
            parent.Value.has (key)
        else
            this.hasOwn key

    member this.set (key: string) (value: obj) =
        if parent.IsSome && parent.Value.hasOwn key then
            parent.Value.set key value |> ignore
        else
            this.variables <- this.variables.Add(key, value)


type Runtime() =
    member this.createEmptyScope() =
        let m = new Map<string, obj>([])
        new Scope(m, option.None)

    member this.evalAstList (body: Ast list) (scope: Scope) =
        async {
            let mutable result = null
            let mutable breakFlag = false
            let mutable i = 0

            while (not breakFlag) && i < body.Length do
                let expr = body[i]
                match expr with
                | ReturnStmt(_) ->
                    breakFlag <- true
                | _ -> ()

                let! value = this.evalAst expr scope
                result <- value

                i <- i + 1

            return result
        }

    member this.reprWrite (value: obj) =
        let openStdout = Console.OpenStandardOutput ()

        if value :? byte array then
            let value: byte array = value |> unbox
            openStdout.Write (value, 0, value.Length) |> ignore
        elif value :? string then
            let value: string = value |> unbox
            printfn "%s" value
        elif value :? Collections.IEnumerable then
            let value: Collections.IEnumerable = value |> unbox
            for item in value do
                this.reprWrite item
        else
            printfn "%A" value

        openStdout.Dispose ()

    member this.createProcess (fileName: string) (args: obj array) =
        async {
            let proc = new Process()

            proc.StartInfo.FileName <- fileName
            proc.StartInfo.RedirectStandardOutput <- true
            proc.StartInfo.UseShellExecute <- false

            for item in args do
                proc.StartInfo.ArgumentList.Add (item.ToString ())

            // TODO: StringBuilderを使う
            let mutable output = ""

            proc.Start() |> ignore

            let openStdout = Console.OpenStandardOutput ()

            let buf: byte array = Array.zeroCreate 256
            while not proc.HasExited do
                let bufSpan = Memory buf
                let! length = (proc.StandardOutput.BaseStream.ReadAsync bufSpan).AsTask () |> Async.AwaitTask
                let out = Text.Encoding.GetEncoding("utf-8").GetString buf[..length]
    
                if length <> 0 then
                    let! _ = openStdout.WriteAsync (buf[..length], 0, length) |> Async.AwaitTask
                    let! _ = openStdout.FlushAsync () |> Async.AwaitTask
    
                    ()
                else
                    ()

                output <- output + out
    
            let! _ = (openStdout.DisposeAsync ()).AsTask () |> Async.AwaitTask

            return Regex.Replace(output, "[\r\n]\x00?$", "")
        }

    member this.evalAst (expr: Ast) (scope: Scope) : Async<obj> =
        async {
            match expr with
            | IdentLiteral(value) ->
                return scope.get value
            | StringLiteral(value) -> return value
            | NumberLiteral(value) -> return value
            | PathLiteral(value) -> return value
            | InvokeExpr(expr, args) ->
                let! value =
                    match expr with
                    | IdentLiteral(ident) ->
                        if (scope.has ident) then
                            async { return scope.get ident }
                        else
                            async { return ident }
                    | _ ->
                        this.evalAst expr scope

                let! argList = List.map (fun x -> this.evalAst x scope) args |> Async.Parallel

                if value :? NativeMethod then
                    let method: NativeMethod = value |> unbox

                    return method.Invoke(argList)
                elif value :? NoshFunctionAsync then
                    let func: NoshFunctionAsync = value |> unbox
                    let! result = func argList

                    return result
                elif value :? NoshFunction then
                    let func: NoshFunction = value |> unbox
                    let result = func argList

                    return result
                else

                let fileName = $"{value}"

                let! output = this.createProcess fileName argList

                return output


            | AddOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?+? y
            | SubOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?-? y
            | MulOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?*? y
            | DivOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?/? y

            | ModOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?%? y

            | AssignOperator(x, y) ->
                match x with
                | IdentLiteral(name) ->
                    let! value = this.evalAst y scope
                    scope.set name value |> ignore

                    return value
                | _ -> return raise (RuntimeError $"cannot assign to {x}")

            | EqualOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x = y

            | NotEqualOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x <> y

            | LtOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?<? y

            | LeOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?<=? y

            | GtOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?>? y

            | GeOperator(x, y) ->
                let! x = this.evalAst x scope
                let! y = this.evalAst y scope

                return x ?>=? y

            | PropertyAccessOperator(x, y) ->
                let! value = this.evalAst x scope
                let key =
                    match y with
                        | IdentLiteral(ident) ->
                            ident
                        | _ -> ""
                let property = value.GetType().GetProperty(key)
                if property <> null then
                    return property.GetValue(x, null)
                else

                let methods = value.GetType().GetMethods() |> Seq.filter (fun x -> x.Name = key) |> Seq.toList
                if methods.Length = 0 then
                    return null
                else
                    return NativeMethod (value, methods)

            | DefineFunctionStmt(name, argNames, body) ->
                let name = name |> Ast.getIdentName

                let argNames = argNames |> List.map (fun x -> x |> Ast.getIdentName)

                let func: NoshFunctionAsync = fun args ->
                    async {
                        let funcScope = Scope(Map.empty, Some(scope))

                        if argNames.Length <> args.Length then
                            return raise (RuntimeError "invalid arguments length")
                        else
                            let mutable i = 0
                            while i < args.Length do
                                let name = argNames[i]
                                let value = args[i]

                                funcScope.set name value

                                i <- i + 1

                        let! result = this.evalAstList body funcScope
                        return result
                    }
                scope.set name func
                return func

            | IfStmt(cond, body, elseBody) ->
                let! value = this.evalAst cond scope
                let value = Convert.ToBoolean value

                if value then
                    let! result = this.evalAstList body scope
                    return result
                else

                match elseBody with
                | Some(elseBody) ->
                    let! result = this.evalAstList elseBody scope
                    return result
                | None ->
                    return null
        }