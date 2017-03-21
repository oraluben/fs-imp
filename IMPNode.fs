module IMPNode

open System
open Microsoft.FSharp.Collections

type Label =
    | LabelID of int
    | EmptyLabel
    member this.ToString =
        match this with
        | LabelID(l) -> sprintf "%d" l
        | EmptyLabel -> "EmptyLabel"
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
    | Co of (Program * Program) * Label
    | Skip of unit * Label
    | Assign of (AName * AExp) * Label
    | Sequence of (CExp * CExp) * Label
    | Wait of BExp * Label
    | If of (BExp * CExp * CExp) * Label
    | While of (BExp * CExp) * Label
    member this.Label =
        match this with
        | Co(_, l) | Skip(_, l) | Assign(_, l) | Sequence(_, l)
        | Wait(_, l) | If(_, l) | While(_, l) -> l
    member this.AsString =
        match this with
        | Co(a, l) -> sprintf "%A Co: %A" l a
        | Sequence(a, l) -> sprintf "%A Sequence: %A" l a
        | Skip(_, l) -> sprintf "%A Skip" l
        | Assign(a, l) -> sprintf "%A Assign: %A" l a
        | Wait(a, l) -> sprintf "%A Wait: %A" l a
        | If(a, l) -> sprintf "%A If: %A" l a
        | While(a, l) -> sprintf "%A While: %A" l a
and Program =
    | Program of CExp * (ProgramLabel * Label)
    member this.ExitLabel =
        match this with
        | Program(_, (_, l)) -> l
    member this.Label =
        match this with
        | Program(c, _) -> c.Label
    override this.ToString() =
        match this with
        | Program(c, _) -> sprintf "%A Program -> %A: %A" this.Label this.ExitLabel c
    member this.AsString = this.ToString()