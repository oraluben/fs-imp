module IMPLogic

open IMPNode

let empty_label : Label = -1
type SameSet =
    | Set of (string)
    | Exclude of (SameSet * string)

type FormulaBool =
    | IsTrue of BExp
    | IsFalse of BExp

type Formula =
    | Conjunction of FormulaBool * FormulaBool
    | Disjunction of FormulaBool * FormulaBool
    | Condition of (string * Label)
    | FAssign of (string * AExp)
    | Bool of FormulaBool
    | Same of SameSet

let True = Bool(IsTrue(BBool(true)))