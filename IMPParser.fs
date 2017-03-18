module IMPParser

open FParsec
open IMPNode

type State =
    {Program: int
     Label: int}
    with
       static member Default = {Program = 0; Label = 0}

let UpdateLabel =
    updateUserState (fun s -> {s with Label = s.Label + 1})
let GetLabel : Parser<int, State> =
    getUserState |>> (fun s -> s.Label)
let UpdateAndGetLabel =
    UpdateLabel >>. GetLabel
let UpdateProgram =
    updateUserState (fun s -> {s with Program = s.Program + 1})
let GetProgram : Parser<int, State> =
    getUserState |>> (fun s -> s.Program)
let UpdateAndGetProgram =
    UpdateProgram >>. GetProgram

let tryApply x f =
    match f with
    | Some(_f) -> _f x
    | None -> x

let impInt =
    anyOf "012" |>> (string >> int >> Int)
let impName = many1Satisfy isLetter
let impAFactor = impInt <|> (impName |>> Name) |> between spaces spaces
let impATermOp = stringReturn "*" TIMES |> between spaces spaces
let impAExprOp =
    anyOf "+-"
    |>> function
        | '+' -> PLUS
        | '-' | _ -> MINUS

let impAExp, impAExpRef = createParserForwardedToRef<AExp, State>()
let impATerm, impATermRef = createParserForwardedToRef<AExp, State>()
do impATermRef :=
    pipe2   impAFactor
            (opt (pipe2 impATermOp impATerm (fun op a2 -> (fun a1 -> AExpression(a1, op, a2)))))
            tryApply
do impAExpRef :=
    pipe2   impATerm
            (opt (pipe2 impAExprOp impAExp (fun op a2 -> (fun a1 -> AExpression(a1, op, a2)))))
            tryApply


let impBCompareOp =
    (stringReturn "="  EQ)
    <|> (stringReturn "<=" LE)
    |> between spaces spaces
let impBAnd = stringReturn "and" AND |> between spaces spaces
let impBOr = stringReturn "or" OR |> between spaces spaces
let impBool =   (stringReturn "True"  true)
            <|> (stringReturn "False" false) |>> BBool
let impCompare =
    pipe3   impAExp impBCompareOp impAExp
            (fun a1 op a2 -> Compare(a1, op, a2))
let impBFactor = impBool <|> impCompare |> between spaces spaces

let impBExp, impBExpRef = createParserForwardedToRef<BExp, State>()
let impBTerm, impBTermRef = createParserForwardedToRef<BExp, State>()
do impBTermRef :=
    pipe2   impBFactor (opt (pipe2 impBAnd impBExp (fun op a2 -> (fun a1 -> Bitop(a1, op, a2)))))
            tryApply

do impBExpRef :=
    pipe2   impBTerm (opt (pipe2 impBOr impBExp (fun op a2 -> (fun a1 -> Bitop(a1, op, a2)))))
            tryApply


let impCExp, impCExpRef = createParserForwardedToRef<CExp, State>()
let impCTerm, impCTermRef = createParserForwardedToRef<CExp, State>()

let LabelCExp (p, l) = Labeled(l, p)
let NewProgram (p, l) = Program(l, p)

let impSkip = stringReturn "skip" Skip
let impWait = pstring "wait" >>. spaces >>. pstring "(" >>. (impBExp |>> Wait) .>> pstring ")"
let impAssign =
    pipe2   (impName .>> (pstring ":=")) impAExp
            (fun n a -> Assign(n, a))
    .>>. UpdateAndGetLabel |>> LabelCExp

let impIf =
    pipe3   ((pstring "if") >>. impBExp)
            ((pstring "then") >>. impCExp .>>. UpdateAndGetLabel |>> LabelCExp)
            ((pstring "else") >>. impCExp .>>. UpdateAndGetLabel .>> (pstring "endif") |>> LabelCExp)
            (fun b p1 p2  -> If(b, p1, p2))
let impWhile =
    pipe2   impBExp ((pstring "do") >>. impCExp)
            (fun b p -> While(b, p))
    |> between (pstring "while") (pstring "endwhile")
    .>>. UpdateAndGetLabel |>> LabelCExp
let impCo =
    pipe2   (impCExp .>>. UpdateAndGetProgram |>> NewProgram)
            ((pstring "||") >>. impCExp .>>. UpdateAndGetProgram |>> NewProgram)
            (fun p1 p2 -> Co(p1, p2))
    |> between (pstring "cobegin") (pstring "coend")
    .>>. UpdateAndGetLabel |>> LabelCExp
let impSequence =
    impCTerm .>>.? ((pstring ";") >>. impCExp .>>. UpdateAndGetLabel |>> LabelCExp)
    |>> Sequence
do impCTermRef :=
    impWhile <|> impIf <|> impCo <|> impSkip <|> impWait <|> impAssign
     |> between spaces spaces
do impCExpRef :=
    impSequence <|> impCTerm

let Program = impCExp |>> (fun p -> Program(0, p)) .>> eof

let test str =
    match runParserOnString Program State.Default "" str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg