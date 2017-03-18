open FParsec

open IMPNode
open IMPParser
open IMPLogic
open IMPKripke

[<EntryPoint>]
let main argv = 
    let lexbuf = """
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
    test lexbuf
    0
