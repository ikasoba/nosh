module Nosh.Program

open System
open System.IO
open Nosh.lang.Parser
open Nosh.lang.runtime.Runtime
open Nosh.cli.Shell

let runtime = new Runtime()
let globalScope = runtime.GlobalScope

if System.Console.IsInputRedirected then
    let state: EvaluateState =
        { redirectStdout = false
          redirectStdin = false }

    let code = stdin.ReadToEnd()
    let expr = parseBy astParser code

    runtime.evalAstList expr globalScope state |> Async.RunSynchronously |> ignore
else
    startShell runtime globalScope
