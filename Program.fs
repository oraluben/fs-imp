open System
open System.IO

open Argu
open FParsec

open IMPNode
open IMPParser
open IMPLogic
open IMPKripke

type IMPArguments =
    | [<MainCommand; ExactlyOnce; Last>] Source of path:string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Source _ -> "specify an IMP source file."

[<EntryPoint>]
let main argv =
    let test str =
        match runParserOnString impProgram State.Default "" str with
        | Success(result, state, _) ->
            printfn "%A" result
            printfn "%A" (BuildKripkeStates result)
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let argv_results = ArgumentParser.Create<IMPArguments>(errorHandler = errorHandler).Parse argv
    let imp = match argv_results.TryGetResult <@ Source @> with
                | Some(s) -> File.ReadAllText(s)
                | None -> "cobegin cobegin skip || skip coend || skip coend"
    test imp
    0
