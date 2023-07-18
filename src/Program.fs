module Nosh.Program

open System.IO
open Nosh.lang.Parser
open Nosh.lang.runtime.Runtime
open Nosh.cli.Shell

let runtime = new Runtime()
let globalScope = runtime.createEmptyScope ()

globalScope.set "ShellPrompt" ((fun _ -> $"{Directory.GetCurrentDirectory()}\n: " |> unbox) :> NoshFunction)
globalScope.set "print" ((fun args -> runtime.reprWrite (unbox<obj> args); null) :> NoshFunction)

if System.Console.IsInputRedirected then
    printfn "------aaa------"
    let code = stdin.ReadToEnd ()
    let expr = parseBy astParser code
    runtime.evalAstList expr globalScope |> Async.RunSynchronously |> printf "%A"
else
    startShell runtime globalScope