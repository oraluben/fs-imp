module IMPLogic

open Microsoft.FSharp.Collections
open IMPNode

type label = int
let empty_label : label = -1
type SameSet =
    | Set of (string)
    | Exclude of (SameSet * string)
    member this.ToString =
        match this with
        | Exclude(s, v) -> sprintf "%s\{%s}" s.ToString v
        | Set(s) -> s

type FormulaBool =
    | IsTrue of BExp
    | IsFalse of BExp

type Formula =
    | Disjunction of (Formula * Formula)
    | Conjunction of (Formula * Formula)
    | Transition of (label * CExp * label)
    | Condition of (string * label)
    | Assign of (string * AExp)
    | Bool of FormulaBool
    | Same of SameSet
    member this.ToString =
        match this with
        | Disjunction(f1, f2) -> sprintf "%s\nor\n%s"
                                        (match f1 with
                                            | Disjunction(f11, f12) -> sprintf "%s" (Disjunction(f11, f12).ToString)
                                            | other -> (sprintf "(\n%s" other.ToString).Replace("\n", "\n ") + "\n)")
                                        (match f2 with
                                            | Disjunction(f21, f22) -> sprintf "%s" (Disjunction(f21, f22).ToString)
                                            | other -> (sprintf "(\n%s" other.ToString).Replace("\n", "\n ") + "\n)")
        | Conjunction(f1, f2) -> sprintf "%s\nand\n%s"
                                        (match f1 with
                                            | Conjunction(f11, f12) -> sprintf "%s" (Conjunction(f11, f12).ToString)
                                            | other -> other.ToString.Replace("\n", "\n "))
                                        (match f2 with
                                            | Conjunction(f21, f22) -> sprintf "%s" (Conjunction(f21, f22).ToString)
                                            | other -> other.ToString.Replace("\n", "\n "))
        | Condition(s, l) -> sprintf "%s = %d" s l
        | Assign(s, a) -> sprintf "%s = %s" s a.ToString
        | Bool(IsTrue(b)) -> sprintf "%s" b.ToString
        | Bool(IsFalse(b)) -> sprintf "!%s" b.ToString
        | Same(s) -> sprintf "same(%s)" s.ToString
        | Transition(ent, c, exit) -> sprintf "%d -> %d: %s" ent exit c.ToString
    member this.DisjunctionNormalForm : Formula =
        match this with
        | Disjunction(f1, f2) -> Disjunction(f1.DisjunctionNormalForm, f2.DisjunctionNormalForm)
        | Conjunction(f1, f2) ->
            match f1 with
            | Disjunction(f11, f12) -> Disjunction(Conjunction(f11, f2).DisjunctionNormalForm, Conjunction(f12, f2).DisjunctionNormalForm)
            | _ ->
                match f2 with
                | Disjunction(f21, f22) -> Disjunction(Conjunction(f1, f21).DisjunctionNormalForm, Conjunction(f1, f22).DisjunctionNormalForm)
                | _ -> Conjunction(f1.DisjunctionNormalForm, f2.DisjunctionNormalForm)
        | _ -> this

let conjunction a =
    let i b = Conjunction(a, b)
    i
let disjunction a =
    let i b = Disjunction(a, b)
    i
let except v =
    let i (s : SameSet) = Exclude(s, v)
    i

let Build (c : CExp) : Formula =
    let mutable cur_label : label = 2
    let rec Extract (f : Formula) : Formula =
        match f with
        | Same(_) | Bool(_) | Assign(_) | Condition(_) -> f
        | Conjunction(f1, f2) -> Conjunction(Extract f1, Extract f2)
        | Disjunction(f1, f2) -> Disjunction(Extract f1, Extract f2)
        | Transition(l1, c, l2) ->
            match c with
            | IMPNode.Assign(n, v) -> Condition("pc", l1)
                                        |> conjunction (Condition("pc'", l2))
                                        |> conjunction (Assign(n, v))
                                        |> conjunction (Same(Set("V") |> except n))
            | IMPNode.Skip(_) -> Condition("pc", l1)
                                        |> conjunction (Condition("pc'", l2))
                                        |> conjunction (Same(Set("V")))
            | Sequence(p1, p2) -> cur_label <- cur_label + 1
                                  Extract(Transition(l1, p1, cur_label - 1))
                                    |> disjunction (Extract (Transition(cur_label - 1, p2, l2)))
            | Wait(b) -> (Condition("pc", l1)
                            |> conjunction (Condition("pc'", l2))
                            |> conjunction (Bool(IsTrue(b)))
                            |> conjunction (Same(Set("V"))))
                         |> disjunction (Condition("pc", l1)
                            |> conjunction (Condition("pc'", l1))
                            |> conjunction (Bool(IsFalse(b)))
                            |> conjunction (Same(Set("V"))))
            | If(b, p1, p2) ->  cur_label <- cur_label + 2
                                (Condition((sprintf "pc%d" l1), l1)
                                    |> conjunction (Condition("pc", cur_label - 2))
                                    |> conjunction (Bool(IsTrue(b)))
                                    |> conjunction (Same(Set("V"))))
                                |> disjunction (Condition("pc", l1)
                                    |> conjunction (Condition("pc'", cur_label - 1))
                                    |> conjunction (Bool(IsFalse(b)))
                                    |> conjunction (Same(Set("V"))))
                                |> disjunction (Extract(Transition(cur_label - 2, p1, l2)))
                                |> disjunction (Extract(Transition(cur_label - 1, p2, l2)))
            | While(b, p) ->    cur_label <- cur_label + 1
                                (Condition("pc", l1)
                                    |> conjunction (Condition("pc'", cur_label - 1))
                                    |> conjunction (Bool(IsTrue(b)))
                                    |> conjunction (Same(Set("V"))))
                                |> disjunction (Condition("pc", l1)
                                    |> conjunction (Condition("pc'", l2))
                                    |> conjunction (Bool(IsFalse(b)))
                                    |> conjunction (Same(Set("V"))))
                                |> disjunction (Extract(Transition(cur_label - 1, p, l2)))
            | Co(p1, p2) -> cur_label <- cur_label + 4
                            let p0_ent = cur_label - 4
                            let p1_ent = cur_label - 3
                            let p0_exit = cur_label - 2
                            let p1_exit = cur_label - 1
                            (Condition("pc", l1)
                                |> conjunction (Condition((sprintf "pc%d'" p0_ent), p0_ent))
                                |> conjunction (Condition((sprintf "pc%d'" p1_ent), p1_ent))
                                |> conjunction (Condition("pc'", empty_label)))
                            |> disjunction (Condition("pc'", empty_label)
                                |> conjunction (Condition((sprintf "pc%d" p0_ent), p0_exit))
                                |> conjunction (Condition((sprintf "pc%d" p1_ent), p1_exit))
                                |> conjunction (Condition("pc'", l2))
                                |> conjunction (Condition((sprintf "pc%d'" p0_ent), empty_label))
                                |> conjunction (Condition((sprintf "pc%d'" p1_ent), empty_label)))
                            |> disjunction (Extract(Transition(p0_ent, p1, p0_exit))
                                |> conjunction (Same(Set(sprintf "pc, pc%d" p1_ent))))
                            |> disjunction (Extract(Transition(p1_ent, p2, p1_exit))
                                |> conjunction (Same(Set(sprintf "pc, pc%d" p0_ent))))
    Transition(0, c, 1) |> Extract