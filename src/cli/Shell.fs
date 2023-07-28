module Nosh.cli.Shell

open System
open System.Reflection
open Nosh.lang.Parser
open Nosh.lang.runtime.Scope
open Nosh.lang.runtime.Runtime

let startShell (runtime: Runtime) (globalScope: Scope) =
    let state: EvaluateState =
        { redirectStdout = false
          redirectStdin = false }

    while true do
        let ps1: NoshFunction = globalScope.get "ShellPrompt" |> unbox

        printf
            "%s"
            (ps1 (
                { input = ()
                  scope = globalScope
                  runtime = runtime },
                [||]
             )
             |> unbox)

        let expr = stdin.ReadLine() |> parseBy astParser

        try
            let result = runtime.evalAstList expr globalScope state |> Async.RunSynchronously
            runtime.reprWrite result
        with
        | :? RuntimeError as err -> printfn "\x1b[31m%s: %s\x1b[0m" (err.GetType().Name) err.Message
        | :? TargetInvocationException as err ->
            printfn
                "\x1b[31m%s: %s\n%s\x1b[0m"
                (err.InnerException.GetType().Name)
                err.InnerException.Message
                err.InnerException.StackTrace
        | _ as err -> printfn "\x1b[31m%s: %s\n%s\x1b[0m" (err.GetType().Name) err.Message err.StackTrace
