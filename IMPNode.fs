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
    | BBool of bool
    | Compare of (AExp * B_COMP_OP * AExp)
    | Bitop of (BExp * B_BIT_OP * BExp)
    | Negative of BExp
    

type CExp =
    | Co of (CExp * CExp)
    | Skip
    | Assign of (AName * AExp)
    | Sequence of (CExp * CExp)
    | Wait of BExp
    | If of (BExp * CExp * CExp)
    | While of (BExp * CExp)
    | Program of (int * CExp)
    | Labeled of (int * CExp)