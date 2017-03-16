open Microsoft.FSharp.Text.Lexing
open IMPNode

[<EntryPoint>]
let main argv = 
    let lexbuf = LexBuffer<char>.FromString """
cobegin
while True do
wait(t=0 | t=1 & t=2);
x:=1+2*1;
t:=1
endwhile
||
while True do
wait(t=1);
x:=2;
t:=0
endwhile;
if True then skip
else skip
endif
coend
    """

    match IMPParser.start IMPLexer.tokenize lexbuf with 
    | { Stmt = s } -> printfn "%s" s.ToString
    0
