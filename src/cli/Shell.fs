module Nosh.cli.Shell

open Nosh.lang.Parser
open Nosh.lang.runtime.Runtime

let startShell (runtime: Runtime) (globalScope: Scope) =
  while true do
    let ps1: NoshFunction = globalScope.get "ShellPrompt" |> unbox
    printf "%s" (ps1 [||] |> unbox)
    let expr = stdin.ReadLine() |> parseBy astParser
    printfn "expr: %A" expr
    runtime.evalAstList expr globalScope |> Async.RunSynchronously |> printfn "%A"