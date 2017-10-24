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
    member this.Reduce (v : Map<string, int>) : int =
        match this with
        | Int(i) -> i
        | Name(n) -> v.[n]
        | AExpression(a1, PLUS, a2) -> (a1.Reduce v) + (a1.Reduce v)
        | AExpression(a1, MINUS, a2) -> (a1.Reduce v) - (a1.Reduce v)
        | AExpression(a1, TIMES, a2) -> (a1.Reduce v) * (a1.Reduce v)

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
    member this.Reduce (v : Map<string, int>) : bool =
        match this with
        | BBool(b) -> b
        | Negative(b) -> not (b.Reduce v)
        | Bitop(b1, AND, b2) -> (b1.Reduce v) && (b2.Reduce v)
        | Bitop(b1, OR, b2) -> (b1.Reduce v) || (b2.Reduce v)
        | Compare(a1, EQ, a2) -> (a1.Reduce v) = (a2.Reduce v)
        | Compare(a1, LE, a2) -> (a1.Reduce v) <= (a2.Reduce v)


[<StructuredFormatDisplay("{AsString}")>]
type CExp =
    | Co of (Program * Program) * Label
    | Skip of unit * Label
    | Assign of (AName * AExp) * Label
    | Sequence of (CExp * CExp) * Label
    | Wait of BExp * Label
    | If of (BExp * CExp * CExp) * Label
    | While of (BExp * CExp) * Label
    member this.Programs : Set<Program> =
        match this with
        | Co((p1, p2), _) -> Set.union p1.Programs p2.Programs
        | Sequence((c1, c2), _) | If((_, c1, c2), _) -> Set.union c1.Programs c2.Programs
        | While((_, c), _) -> c.Programs
        | _ -> Set.empty
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
    member this.CExp : CExp =
        match this with
        | Program(c, _) -> c
    member this.ProgramLabel =
        match this with
        | Program(_, (l, _)) -> l
    member this.ExitLabel =
        match this with
        | Program(_, (_, l)) -> l
    member this.Label =
        match this with
        | Program(c, _) -> c.Label
    member this.Programs : Set<Program> =
        Set.add this this.CExp.Programs
    member this.ToString =
        match this with
        | Program(c, _) -> sprintf "Program %A (%A -> %A): %A" this.ProgramLabel this.Label this.ExitLabel c
