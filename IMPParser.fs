module IMPParser

open FParsec
open IMPNode

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

let impAExp, impAExpRef = createParserForwardedToRef<AExp, unit>()
let impATerm, impATermRef = createParserForwardedToRef<AExp, unit>()
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

let impBExp, impBExpRef = createParserForwardedToRef<BExp, unit>()
let impBTerm, impBTermRef = createParserForwardedToRef<BExp, unit>()
do impBTermRef :=
    pipe2   impBFactor (opt (pipe2 impBAnd impBExp (fun op a2 -> (fun a1 -> Bitop(a1, op, a2)))))
            tryApply

do impBExpRef :=
    pipe2   impBTerm (opt (pipe2 impBOr impBExp (fun op a2 -> (fun a1 -> Bitop(a1, op, a2)))))
            tryApply


let impCExp, impCExpRef = createParserForwardedToRef<CExp, unit>()
let impCTerm, impCTermRef = createParserForwardedToRef<CExp, unit>()

let impSkip = stringReturn "skip" Skip
let impWait = pstring "wait" >>. spaces >>. pstring "(" >>. (impBExp |>> Wait) .>> pstring ")"
let impAssign =
    pipe3   impName (pstring ":=") impAExp
            (fun n _ a -> Assign(n, a))
let impIf =
    pipe5   impBExp (pstring "then") impCExp (pstring "else") impCExp
            (fun b _ p1 _ p2 -> If(b, p1, p2))
    |> between (pstring "if") (pstring "endif")
let impWhile =
    pipe3   impBExp (pstring "do") impCExp
            (fun b _ p -> While(b, p))
    |> between (pstring "while") (pstring "endwhile")
let impCo =
    pipe3   impCExp (pstring "||") impCExp
            (fun p1 _ p2 -> Co(p1, p2))
    |> between (pstring "cobegin") (pstring "coend")
let impSequence =
    impCTerm .>>.? (pipe2 (pstring ";") impCExp (fun _ p2 -> p2)) |>> Sequence

do impCTermRef :=
    impWhile <|> impIf <|> impCo <|> impSkip <|> impWait <|> impAssign
     |> between spaces spaces
do impCExpRef :=
    impSequence <|> impCTerm

let Program = impCExp .>> eof

let test str =
    match run Program str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg