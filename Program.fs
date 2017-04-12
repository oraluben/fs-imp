open FParsec

open IMPNode
open IMPParser
open IMPLogic
open IMPKripke

[<EntryPoint>]
let main argv = 
    let mutable lexbuf = """
cobegin
while True do
wait(turn=0);
x:=1;
turn:=1
endwhile
||
while True do
wait(turn=1);
x:=2;
turn:=0
endwhile
coend
    """
    lexbuf <- "cobegin cobegin skip || skip coend || skip coend"

    let test str =
        match runParserOnString impProgram State.Default "" str with
        | Success(result, state, _) ->
            printfn "Success!"
            printfn "%s" result.ToString
            printfn "%A" state.SharedVar
            printfn "%A" (Build result).DisjunctionNormalForm
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    test lexbuf
    0
