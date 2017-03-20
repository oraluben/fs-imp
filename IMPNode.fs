module IMPNode

open System
open Microsoft.FSharp.Collections

type Label = int
type ProgramLabel = Label

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
    member this.Names =
        match this with
        | Int(_) -> Set.empty
        | Name(n) -> Set.singleton n
        | AExpression(a1, _, a2) -> Set.union a1.Names a2.Names

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
    member this.AVar =
        match this with
        | BBool(_) -> Set.empty
        | Negative(b) -> b.AVar
        | Bitop(b1, _, b2) -> Set.union b1.AVar b2.AVar
        | Compare(a1, _, a2) -> Set.union a1.Names a2.Names


[<StructuredFormatDisplay("{AsString}")>]
type CExp =
    | Co of (CExp * CExp) * Label
    | Skip of unit * Label
    | Assign of (AName * AExp) * Label
    | Sequence of (CExp * CExp) * Label
    | Wait of BExp * Label
    | If of (BExp * CExp * CExp) * Label
    | While of (BExp * CExp) * Label
    | Program of (CExp * Label) * ProgramLabel
    member this.Label =
        match this with
        | Co(_, l) | Skip(_, l) | Assign(_, l) | Sequence(_, l)
        | Wait(_, l) | If(_, l) | While(_, l) | Program(_, l)
            -> l
    override this.ToString() =
        match this with
        | Program((p, exit_label), program_label) -> sprintf "%d Program -> %d: %A" program_label exit_label p
        | Co(a, l) -> sprintf "%d Co: %A" l a
        | Sequence(a, l) -> sprintf "%d Sequence: %A" l a
        | Skip(_, l) -> sprintf "%d Skip" l
        | Assign(a, l) -> sprintf "%d Assign: %A" l a
        | Wait(a, l) -> sprintf "%d Wait: %A" l a
        | If(a, l) -> sprintf "%d If: %A" l a
        | While(a, l) -> sprintf "%d While: %A" l a
    member this.AsString = this.ToString()
