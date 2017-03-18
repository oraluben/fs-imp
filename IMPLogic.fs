module IMPLogic

open IMPNode

type label = int
let empty_label : label = -1
type SameSet =
    | Set of (string)
    | Exclude of (SameSet * string)

type FormulaBool =
    | IsTrue of BExp
    | IsFalse of BExp