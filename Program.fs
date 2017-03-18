open Microsoft.FSharp.Text.Lexing
open IMPNode
open IMPLogic
open IMPKripke

[<EntryPoint>]
let main argv = 
    let lexbuf = LexBuffer<char>.FromString """
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

    let f = match IMPParser.start IMPLexer.tokenize lexbuf with
            | { Stmt = s } -> Build s
    do printf "%s\n" f.DisjunctionNormalForm.ToString
    do printf "%A\n" f.sharedVars
    0
