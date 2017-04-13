module IMPKripke

open Microsoft.FSharp.Collections
open IMPNode
open IMPLogic
open IMPParser

type Valuation = Map<string, int>
let checkVal (vs : Valuation) (fb : FormulaBool) : bool =
    match fb with
    | IsTrue(b) -> (b.Reduce vs)
    | IsFalse(b) -> not (b.Reduce vs)
type Pc = Map<ProgramLabel, Label>
type KripkeState = Valuation * Pc
let Vs (kstate : KripkeState) =
    match kstate with
    | (v, _) -> v
let Pcs (kstate : KripkeState) =
    match kstate with
    | (_, p) -> p
type KripkeStructure = Set<KripkeState>

let ProgramIDs (p : Program) : Set<ProgramLabel> =
    Set.map (fun (c : Program) -> c.ProgramLabel) p.Programs

let SharedVars (p : Program) =
    let mutable Vars : Map<string, Set<ProgramLabel>> = Map.empty
    let mutable ProgramStack : Stack<ProgramLabel> = StackNode(EmptyStack, p.ProgramLabel)

    let rec walk (c : CExp) =
        let ProgramLabel =
            match ProgramStack with
            | StackNode(_, l) -> l
            | _ -> "stack empty" |> failwith
        match c with
        | Skip(_) | Wait(_) -> ()
        | Assign((name, _), _) ->
            Vars <- Map.add
                    name
                    (match (Map.tryFind name Vars) with
                    | None -> Set.singleton ProgramLabel
                    | Some(set) -> Set.add ProgramLabel set)
                    Vars
        | Sequence((c1, c2), _) | If((_, c1, c2), _) -> (walk(c1), walk(c2)) |> ignore
        | While((_, c), _)  -> walk(c) |> ignore
        | Co((p1, p2), _) ->
            ProgramStack <- StackNode(ProgramStack, p1.ProgramLabel)
            walk(p1.CExp)
            ProgramStack <-
                match ProgramStack with
                | StackNode(s, _) -> s
                | _ -> "stack empty" |> failwith
            ProgramStack <- StackNode(ProgramStack, p2.ProgramLabel)
            walk(p2.CExp)
            ProgramStack <-
                match ProgramStack with
                | StackNode(s, _) -> s
                | _ -> "stack empty" |> failwith
    walk p.CExp
    Map.fold (fun cur k (ps : Set<ProgramLabel>) -> if (ps.Count > 0) then Set.union (Set.singleton k) cur else cur) Set.empty Vars

let InitStates (p : Program) : KripkeStructure =
    let init_ps : Pc = Set.fold (fun cur_ps cur_pc ->
                                    Map.add cur_pc (if cur_pc = p.ProgramLabel then p.Label else EmptyLabel) cur_ps) Map.empty (ProgramIDs p)
    let vals n : Set<string * int> = Seq.fold (fun (cur_n_v_set : Set<string * int>) v -> cur_n_v_set.Add (n, v)) Set.empty {0 .. 1}
    let cartesianProduct (kstate : KripkeState) (n : string) : KripkeStructure =
        Set.map (fun (v_name, v_val) -> match kstate with
                                        | (v, pc) -> (Map.add v_name v_val v), pc) (vals n)
    let addSharedVar (ks : KripkeStructure) (v : string) : KripkeStructure =
        Set.fold (fun cur_new_ks kstate -> Set.union cur_new_ks (cartesianProduct kstate v)) Set.empty ks 
    Set.fold (fun (cur_v_p_set : KripkeStructure) v -> addSharedVar cur_v_p_set v) (Set.singleton (Map.empty, init_ps)) (SharedVars p)

let Next (kstate : KripkeState) (fs : Formula) : KripkeStructure =
    let tryNext (kstate : KripkeState) (f : Formula) : Option<KripkeState> =
        // f is a Conjunction
        let rec allCon (f : Formula) : Set<Formula> =
            match f with
            | Bool(_) | Same(_) |  FAssign(_) | PcAt(_,_) -> Set.singleton f
            | Conjunction(f1, f2) -> Set.union (allCon f1) (allCon f2)
            | Disjunction(_) -> "error" |> failwith
        // check
        if Set.fold (fun pre cur_f -> pre && match cur_f with
                                                | PcAt(EnterLabel(l), v) ->
                                                    //printfn "%A %A %A" (Pcs kstate) l v
                                                    (Pcs kstate).[LabelID(l)] = v
                                                | Bool(fb) ->
                                                    //printfn "%A %A %A" (Vs kstate) fb (checkVal (Vs kstate) fb)
                                                    checkVal (Vs kstate) fb
                                                | _ -> true) true (allCon f)
        then
            Some(Set.fold (fun pre cur_f -> match cur_f with
                                            | PcAt(ExitLabel(l), v) -> (Vs pre), (Map.add (LabelID l) v (Pcs pre))
                                            | FAssign(n, v) -> (Map.add n (v.Reduce (Vs pre)) (Vs pre)), (Pcs pre)
                                            | _ -> pre) kstate (allCon f))
        else
            None
    Set.fold (fun s f -> match (tryNext kstate f) with
                            | Some(ks) -> Set.add ks s
                            | None -> s) Set.empty fs.Conjunctions

let rec BuildKripkeStates (p : Program) : Set<KripkeState * KripkeState> =
    let rules = (Build p).DisjunctionNormalForm
    let mutable todos : Set<KripkeState> = InitStates p
    let mutable done_ : Set<KripkeState> = Set.empty
    let mutable graph : Set<KripkeState * KripkeState> = Set.empty
    while not todos.IsEmpty do
        let curs = Set.maxElement todos
        Set.iter (fun next_kstate ->
                            if Set.contains next_kstate done_ then () else todos <- Set.add next_kstate todos
                            if curs = next_kstate then printfn "%A" next_kstate
                            graph <- Set.add (curs, next_kstate) graph) (Next curs rules)
        done_ <- Set.add curs done_
        todos <- Set.remove curs todos
    graph