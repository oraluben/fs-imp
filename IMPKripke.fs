module IMPKripke

open Microsoft.FSharp.Collections
open IMPNode
open IMPLogic

type state = int
type KripkeStructures =
    (state * Set<state> * Set<state * state>)