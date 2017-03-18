module IMPNode

open System

type AName = string
type A_OP = 
    | PLUS | MINUS | TIMES
    member this.ToString =
        match this with
        | PLUS -> "+"
        | MINUS -> "-"
        | TIMES -> "*"
type AExp = 
    | Int of int
    | Name of AName
    | AExpression of (AExp * A_OP * AExp)
    member this.ToString : string =
        match this with
        | Int(i) -> sprintf "%d" i
        | Name(n) -> n
        | AExpression(a1, op, a2) -> sprintf "(%s %s %s)" a1.ToString op.ToString a2.ToString

type B_BIT_OP =
    | AND | OR
    member this.ToString : string =
        match this with
        | AND -> "&"
        | OR -> "|"
type B_COMP_OP =
    | EQ | LE
    member this.ToString : string =
        match this with
        | EQ -> "="
        | LE -> "<="
type BExp = 
    | Bool of bool
    | Compare of (AExp * B_COMP_OP * AExp)
    | Bitop of (BExp * B_BIT_OP * BExp)
    | Negative of BExp
    member this.ToString : string =
        match this with
        | Bool(b) -> sprintf "%b" b
        | Compare(a1, op, a2) -> sprintf "(%s %s %s)" a1.ToString op.ToString a2.ToString
        | Bitop(b1, op, b2) -> sprintf "(%s %s %s)" b1.ToString op.ToString b2.ToString
        | Negative(b) -> sprintf "(!%s)" b.ToString 
    

type CExp =
    | Co of (CExp * CExp)
    | Skip
    | Assign of (AName * AExp)
    | Sequence of (CExp * CExp)
    | Wait of BExp
    | If of (BExp * CExp * CExp)
    | While of (BExp * CExp)
    member this.ToString : string =
        match this with
        | Skip -> "skip"
        | Co(c1, c2) -> (sprintf "co\n%s\n%s" c1.ToString c2.ToString).Replace("\n", "\n ")
        | Assign(name, expr) -> sprintf "%s := %s" name expr.ToString
        | Sequence(c1, c2) -> sprintf "%s\n%s" c1.ToString c2.ToString
        | Wait(b) -> sprintf "wait(%s)" b.ToString
        | If(b, c1, c2) -> (sprintf "if %s\n%s\n%s" b.ToString c1.ToString c2.ToString).Replace("\n", "\n ")
        | While(b, c) -> (sprintf "while %s\n%s" b.ToString c.ToString).Replace("\n", "\n ")

type Program = 
    { Stmt: CExp }