module GraphViz

open System
open System.IO

open IMPNode
open IMPKripke

// ================================
// Generate the graph using GraphViz
// ================================


// change this as needed for your local environment
let graphVizPath = @"D:\Program\graphviz-2.38\release\bin\"

let toCsv sep strList = 
    match strList with
    | [] -> ""
    | _ -> List.reduce (fun s1 s2 -> s1 + sep + s2) strList 

let getKripkeStateName (ks : KripkeState) : string =
    let labelName (l : Label) =
        match l with
        | LabelID(l) -> sprintf "%d" l
        | EmptyLabel -> "ʌ"
    match ks with
    | (v, p) ->
        sprintf "\"%s\n%s\"" ((Map.fold (fun s k v -> (sprintf "%s=%d" k v) :: s) List.empty v) |> String.concat ",") ((Map.fold (fun s k v -> (sprintf "pc%s=%s" (labelName k) (labelName v)) :: s) List.empty p) |> String.concat ",")

let writeDepSet writer (set : KripkeState * KripkeState) =
    match set with
    | (ks1, ks2) -> fprintfn writer "   %s -> { %s }" (getKripkeStateName ks1) (getKripkeStateName ks2)
    

// Create a DOT file for graphviz to read.
let createDotFile dotFilename depSets = 
    use writer = new StreamWriter(path=dotFilename)
    fprintfn writer "digraph G {"
    depSets
    |> Seq.iter (writeDepSet writer)
    
    let fs = (Set.map (fun s -> match s with | (f, _) -> f) depSets)
    let ts = (Set.map (fun s -> match s with | (_, t) -> t) depSets)
    let mutable begins = Set.difference fs ts
    fprintfn writer "{rank = same; %s}" (List.map getKripkeStateName  (begins |> Seq.toList) |> String.concat "; ")
    while not (begins = (Set.union fs ts)) do
        let lev = Set.filter (fun (f, t) -> (Set.contains f begins) && not (Set.contains t begins)) depSets |> Set.map (fun (_, t) -> t)
        fprintfn writer "{rank = same; %s}" (List.map getKripkeStateName  (lev |> Seq.toList) |> String.concat "; ")
        begins <- Set.union begins lev
    fprintfn writer "   }"

// shell out to run a command line program
let startProcessAndCaptureOutput cmd cmdParams = 
    let debug = false

    if debug then
        printfn "Process: %s %s" cmd cmdParams 
    let si = new System.Diagnostics.ProcessStartInfo(cmd, cmdParams)
    si.UseShellExecute <- false
    si.RedirectStandardOutput <- true
    use p = new System.Diagnostics.Process()
    p.StartInfo <- si
    if p.Start() then
        if debug then
            use stdOut = p.StandardOutput
            stdOut.ReadToEnd() |> printfn "%s" 
            printfn "Process complete"
    else
        printfn "Process failed"

/// Generate an image file from a DOT file
/// algo = dot, neato
/// format = gif, png, jpg, svg
let generateImageFile dotFilename algo format imgFilename =
    let cmd = sprintf @"""%s%s.exe""" graphVizPath algo 
    let inFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__,dotFilename)
    let outFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__,imgFilename)
    let cmdParams = sprintf "-T%s -o\"%s\" \"%s\"" format outFile inFile
    startProcessAndCaptureOutput cmd cmdParams