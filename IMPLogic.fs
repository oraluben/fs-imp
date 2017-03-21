module IMPLogic

open IMPNode
open IMPParser

type SameSet =
    | Set of (string)
    | Exclude of (SameSet * string)

type FormulaBool =
    | IsTrue of BExp
    | IsFalse of BExp
    
[<StructuredFormatDisplay("{AsString}")>]
type Formula =
    | Conjunction of Formula * Formula
    | Disjunction of Formula * Formula
    | Condition of (string * Label)
    | FAssign of (string * AExp)
    | Bool of FormulaBool
    | Same of SameSet
    | PcAt of string
    member this.DisjunctionNormalForm =
        match this with
        | Bool(_) | Same(_) | PcAt(_) | FAssign(_) | Condition(_) -> this
        | Conjunction(Disjunction(d1, d2), f) | Conjunction(f, Disjunction(d1, d2)) ->
            Disjunction(Conjunction(d1, f), Conjunction(d2, f)).DisjunctionNormalForm
        | Conjunction(f1, f2) -> Conjunction(f1.DisjunctionNormalForm, f2.DisjunctionNormalForm)
        | Disjunction(f1, f2) -> Disjunction(f1.DisjunctionNormalForm, f2.DisjunctionNormalForm)
    member this.AsString =
        match this with
        | Bool(b) -> sprintf "%A" b
        | Same(s) -> sprintf "Same(%A)" s
        | PcAt(l) -> sprintf "pc = %s" l
        | FAssign(v, a) -> sprintf "%s = %A" v a
        | Condition(v, l) -> sprintf "%s = %A" v l
        | Conjunction(f, Conjunction(c1, c2)) | Conjunction(Conjunction(c1, c2), f) ->
            sprintf "(%A) and %A" f (Conjunction(c1, c2))
        | Conjunction(f1, f2) -> sprintf "(%A) and (%A)" f1 f2
        | Disjunction(f1, f2) -> sprintf "%A\nor %A" f1 f2

let True =  Bool(IsTrue(BBool(true)))
let False = Bool(IsTrue(BBool(false)))

let conjunction f1 f2 = (f1, f2) |> Conjunction
let disjunction f1 f2 = (f1, f2) |> Disjunction

let rec Build (p : Program) : Formula =
    let mutable ProgramStack : Stack<ProgramLabel> = EmptyStack

    let rec BuildFromTransition (p : CExp, exit_label : Label) =
        let program_label =
            match ProgramStack with
            | StackNode(_, l) -> l
            | EmptyStack ->
                sprintf "no program label: %A" p |> failwith
        let pc0 = (if program_label = LabelID(0)
                    then "pc"
                    else sprintf "pc%s" program_label.ToString)
        let pc1 = (if program_label = LabelID(0)
                    then "pc'"
                    else sprintf "pc%s'" program_label.ToString)
        match p with
        | Skip(_, l) ->
            Condition(pc0, program_label)
                |> conjunction (Condition(pc1, exit_label))
                |> conjunction (Same(Set("V")))
        | Wait(b, l) ->
            (Condition(pc0, program_label)
                |> conjunction (Condition(pc1, program_label))
                |> conjunction (Bool(IsFalse(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (Condition(pc0, program_label)
                |> conjunction (Condition(pc1, exit_label))
                |> conjunction (Bool(IsTrue(b)))
                |> conjunction (Same(Set("V"))))
        | Assign((n, a), l) ->
            Condition(pc0, program_label)
                |> conjunction (Condition(pc1, exit_label))
                |> conjunction (FAssign(n, a))
                |> conjunction (Same(Exclude(Set("V"), n)))
        | Sequence((c1, c2), l) ->
            BuildFromTransition(c1, c2.Label)
                |> disjunction (BuildFromTransition(c2, exit_label))
        | If((b, c1, c2), l) ->
            (Condition(pc0, program_label)
                |> conjunction (Condition(pc1, c1.Label))
                |> conjunction (Bool(IsTrue(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (Condition(pc0, program_label)
                |> conjunction (Condition(pc1, c2.Label))
                |> conjunction (Bool(IsFalse(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (BuildFromTransition(c1, exit_label))
            |> disjunction (BuildFromTransition(c2, exit_label))
        | While((b, c), l) ->
            (Condition(pc0, program_label)
                |> conjunction (Condition(pc1, c.Label))
                |> conjunction (Bool(IsTrue(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (Condition(pc0, program_label)
                |> conjunction (Condition(pc1, exit_label))
                |> conjunction (Bool(IsFalse(b)))
                |> conjunction (Same(Set("V"))))
            |> disjunction (BuildFromTransition(c, program_label))
        | Co((Program(c0, (pl0, l0)), Program(c1, (pl1, l1))), l) ->
            let pc00 = sprintf "pc%s" pl0.ToString
            let pc01 = sprintf "pc%s'" pl0.ToString
            let pc10 = sprintf "pc%s" pl1.ToString
            let pc11 = sprintf "pc%s'" pl1.ToString
            (Condition(pc0, program_label)
                |> conjunction (Condition(pc00, c0.Label))
                |> conjunction (Condition(pc10, c1.Label))
                |> conjunction (Condition(pc1, EmptyLabel)))
            |> disjunction (Condition(pc0, EmptyLabel)
                |> conjunction (Condition(pc00, l0))
                |> conjunction (Condition(pc10, l1))
                |> conjunction (Condition(pc1, exit_label))
                |> conjunction (Condition(pc01, EmptyLabel))
                |> conjunction (Condition(pc11, EmptyLabel)))
            |> disjunction (BuildFromTransition(c0, c0.Label)
                |> conjunction (PcAt pc00))
            |> disjunction (BuildFromTransition(c1, c1.Label)
                |> conjunction (PcAt pc10))
    match p with
    | Program(c, (program_label, exit_label)) ->
        (ProgramStack <- StackNode(ProgramStack, program_label))
        |> (fun _ -> (c, exit_label) |> BuildFromTransition)
        |> (ProgramStack <- match ProgramStack with
                            | StackNode(s, _) -> s
                            | EmptyStack -> EmptyStack
            id)
let rec Y f x = f (Y f) x