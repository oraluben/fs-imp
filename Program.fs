open Microsoft.FSharp.Text.Lexing
open IMPNode
open IMPLogic

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

    match IMPParser.start IMPLexer.tokenize lexbuf with 
    | { Stmt = s } -> printf "%s\n" (Build s).DisjunctionNormalForm.ToString
    0
