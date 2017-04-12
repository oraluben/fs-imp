module IMPKripke

open Microsoft.FSharp.Collections
open IMPNode
open IMPLogic

type Valuation = Map<string, int>
type Pc = Map<ProgramLabel, Label>
type KripkeState = Valuation * Pc
type KripkeStructure = Set<KripkeState>

let t : KripkeState = Map.empty, Map.empty

let rec BuildKripkeStates = True