module IMPLogic

open IMPNode

let empty_label : Label = -1
type SameSet =
    | Set of (string)
    | Exclude of (SameSet * string)

type FormulaBool =
    | IsTrue of BExp
    | IsFalse of BExp
    | Conjunction of FormulaBool * FormulaBool
    | Disjunction of FormulaBool * FormulaBool