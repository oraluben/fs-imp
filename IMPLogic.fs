module IMPLogic

open IMPNode
open IMPParser

type SameSet =
    | Set of (string)
    | Exclude of (SameSet * string)

type FormulaBool =
    | IsTrue of BExp
    | IsFalse of BExp

[<StructuredFormatDisplay("{ToString}")>]
type FLabel =
    | EnterLabel of int
    | ExitLabel of int
    member this.ToString =
        match this with
        | EnterLabel(l) -> sprintf "%d" l
        | ExitLabel(l) -> sprintf "%d'" l

[<StructuredFormatDisplay("{ToString}")>]
type Formula =
    | Conjunction of Formula * Formula
    | Disjunction of Formula * Formula
    | FAssign of (string * AExp)
    | Bool of FormulaBool
    | Same of SameSet
    | PcAt of FLabel * Label
    member this.DisjunctionNormalForm =
        match this with
        | Bool(_) | Same(_) |  FAssign(_) | PcAt(_,_) -> this
        | Conjunction(Disjunction(d1, d2), f) | Conjunction(f, Disjunction(d1, d2)) ->
            Disjunction(Conjunction(d1, f), Conjunction(d2, f)).DisjunctionNormalForm
        | Conjunction(f1, f2) -> Conjunction(f1.DisjunctionNormalForm, f2.DisjunctionNormalForm)
        | Disjunction(f1, f2) -> Disjunction(f1.DisjunctionNormalForm, f2.DisjunctionNormalForm)
    member this.Conjunctions : Set<Formula> =
        match this.DisjunctionNormalForm with
        | Disjunction(f1, f2) -> Set.union f1.Conjunctions f2.Conjunctions
        | f -> Set.singleton f
    member this.ToString =
        match this with
        | Bool(b) -> sprintf "%A" b
        | Same(s) -> sprintf "Same(%A)" s
        | FAssign(v, a) -> sprintf "%s = %A" v a
        | Conjunction(f, Conjunction(c1, c2)) | Conjunction(Conjunction(c1, c2), f) ->
            sprintf "(%A) and %A" f (Conjunction(c1, c2))
        | Conjunction(f1, f2) -> sprintf "(%A) and (%A)" f1 f2
        | Disjunction(f1, f2) -> sprintf "%A\nor %A" f1 f2
        | PcAt(p, l) -> sprintf "pc%A = %A" p l

let True =  Bool(IsTrue(BBool(true)))
let False = Bool(IsTrue(BBool(false)))

let conjunction f1 f2 = (f1, f2) |> Conjunction
let disjunction f1 f2 = (f1, f2) |> Disjunction

let rec Build (p : Program) : Formula =
    let mutable ProgramStack : Stack<ProgramLabel> = EmptyStack

    let rec BuildFromTransition (p : CExp, exit_label : Label) =
        let program_label =
            match ProgramStack with
            | StackNode(_, l) ->
                match l with
                | LabelID(l) -> l
                | EmptyLabel ->
                    sprintf "empty label in program stack" |> failwith
            | EmptyStack ->
                sprintf "no program label: %A" p |> failwith
        let pc0 = program_label
        let pc1 = program_label
        match p with
        | Skip(_, l) ->
            PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), exit_label))
                |> conjunction (Same(Set("V")))
        | Wait(b, l) ->
            (PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), p.Label))
                |> conjunction (Bool(IsFalse(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), exit_label))
                |> conjunction (Bool(IsTrue(b)))
                |> conjunction (Same(Set("V"))))
        | Assign((n, a), l) ->
            PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), exit_label))
                |> conjunction (FAssign(n, a))
                |> conjunction (Same(Exclude(Set("V"), n)))
        | Sequence((c1, c2), l) ->
            BuildFromTransition(c1, c2.Label)
                |> disjunction (BuildFromTransition(c2, exit_label))
        | If((b, c1, c2), l) ->
            (PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), c1.Label))
                |> conjunction (Bool(IsTrue(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), c2.Label))
                |> conjunction (Bool(IsFalse(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (BuildFromTransition(c1, exit_label))
            |> disjunction (BuildFromTransition(c2, exit_label))
        | While((b, c), l) ->
            (PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), c.Label))
                |> conjunction (Bool(IsTrue(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), exit_label))
                |> conjunction (Bool(IsFalse(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (BuildFromTransition(c, p.Label))
        | Co((Program(c0, (pl0, l0)), Program(c1, (pl1, l1))), l) ->
            let pl0i =
                match pl0 with
                | LabelID(l) -> l
                | EmptyLabel ->
                    sprintf "empty label in program %A" c0 |> failwith
            let pl1i =
                match pl1 with
                | LabelID(l) -> l
                | EmptyLabel ->
                    sprintf "empty label in program %A" c1 |> failwith
            (PcAt(EnterLabel(program_label), p.Label)
                |> conjunction (PcAt(ExitLabel(program_label), EmptyLabel))
                |> conjunction (PcAt(ExitLabel(pl0i), c0.Label))
                |> conjunction (PcAt(ExitLabel(pl1i), c1.Label)))
            |> disjunction (PcAt(EnterLabel(program_label), EmptyLabel)
                |> conjunction (PcAt(EnterLabel(pl0i), l0))
                |> conjunction (PcAt(EnterLabel(pl1i), l1))
                |> conjunction (PcAt(ExitLabel(program_label), exit_label))
                |> conjunction (PcAt(ExitLabel(pl0i), EmptyLabel))
                |> conjunction (PcAt(ExitLabel(pl1i), EmptyLabel)))
            |> (fun f ->
                ProgramStack <- StackNode(ProgramStack, LabelID(pl0i))
                disjunction (BuildFromTransition(c0, c0.Label)
                    |> (ProgramStack <- match ProgramStack with
                                        | StackNode(s, _) -> s
                                        | EmptyStack -> EmptyStack
                        id)
                    |> conjunction (Same(Set(sprintf "pc%d, pc%d" program_label pl1i)))) f)
            |> (fun f ->
                ProgramStack <- StackNode(ProgramStack, LabelID(pl1i))
                disjunction (BuildFromTransition(c1, c1.Label)
                    |> (ProgramStack <- match ProgramStack with
                                        | StackNode(s, _) -> s
                                        | EmptyStack -> EmptyStack
                        id)
                    |> conjunction (Same(Set(sprintf "pc%d, pc%d" program_label pl0i)))) f)
    match p with
    | Program(c, (program_label, exit_label)) ->
        (ProgramStack <- StackNode(ProgramStack, program_label))
        |> (fun _ -> (c, exit_label) |> BuildFromTransition)
        |> (ProgramStack <- match ProgramStack with
                            | StackNode(s, _) -> s
                            | EmptyStack -> EmptyStack
            id)